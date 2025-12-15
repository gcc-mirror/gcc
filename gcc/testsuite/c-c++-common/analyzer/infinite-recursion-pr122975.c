void fun(int x, int y)
{
  if (x + y != 0) 
    fun(x, -x); /* { dg-bogus "infinite recursion" } */
}
