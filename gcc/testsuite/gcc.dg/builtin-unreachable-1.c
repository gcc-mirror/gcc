/* Check that __builtin_unreachable() prevents the 'control reaches
   end of non-void function' diagnostic.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Wreturn-type" } */
int
f(int a, int b)
{
  if (a)
    {
      return b;
    }
  else
    {
      asm ("bug");
      __builtin_unreachable();
    }
}
