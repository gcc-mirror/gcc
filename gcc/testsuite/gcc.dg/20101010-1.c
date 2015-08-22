/* { dg-do compile } */
/* { dg-options "-O2 -fno-crossjumping" } */
/* { dg-require-effective-target alloca } */

int bar1 ();
int bar2 ();

int foo (void)
{
  int len;
  if (bar1 (&len))
    {
      char devpath [len];
      if (bar2 (devpath) == len)
        return len;
    }
  return -1;
}
