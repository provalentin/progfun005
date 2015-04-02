package recfun
import common._

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
    
   println( "4: " + countChange(4, List(1,2)));
   
   println("balance:  " + balance("(((hello)there)you)(".toList));
   println("check:  " + check("(((hello)there)you)(".toList, 0));
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    //println("(" + c + "," + r + ")");
    if(c == 0 || r == 0 || c == r) 1 else pascal(c, r -1) + pascal(c - 1, r - 1);
  }
  
  //println("result: " + pascal(1,3));
  
  

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = check(chars, 0) == 0
  
  def check(c: List[Char], b: Int):Int = {
    println(c + " : " + b);
    if(b<0) b else if(c.isEmpty) b else check(c.tail, b + { if(c.head == '(') 1 else if(c.head == ')') -1 else 0})
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def count_change(amount: Int, n: Int): Int = {
      //println("a: " + amount + " n:" + n);
      if(amount==0) 1 else
        if(amount <0 || n == 0) 0 else
          count_change(amount, n - 1) + count_change(amount - coins(n-1), n)
      
    }
    count_change(money, coins.size)
  }
  
  
  
}
