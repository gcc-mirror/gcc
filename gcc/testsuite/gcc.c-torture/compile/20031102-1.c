/* PR optimization/10817.
   Check that the following code doesn't cause any problems
   for GCC's if-conversion passes.  */

int foo(int t)
{
  int result = 0;
  if (t != 0)
    result = t;
  return result;
}

