/* Test that gcc understands that the call to g might clobber i.  */

/* { dg-do run } */
/* { dg-options "-O2 -fgnu89-inline" } */

__inline int f ()
{
  static int i;
  int i2 = i;
  i = i2 + 1;
  return i;
}

int g () { return f (); }

int main ()
{
  if (f() != 1
      || g() != 2
      || f() != 3)
    return 1;
  return 0;
}
