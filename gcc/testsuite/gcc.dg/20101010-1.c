/* { dg-do compile } */
/* { dg-options "-O2 -fno-crossjumping" } */

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
