/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "x_\[0-9]+\\\(D\\\) & y_\[0-9]+\\\(D\\\);" "optimized" } } */
/* { dg-final { scan-tree-dump-not "x_\[0-9]+\\\(D\\\) \\| y_\[0-9]+\\\(D\\\);" "optimized" } } */
/* { dg-final { scan-tree-dump-times "x_\[0-9]+\\\(D\\\) \\^ y_\[0-9]+\\\(D\\\);" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "_\[0-9] \\^ _\[0-9]" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "~_\[0-9]\+" 8 "optimized" } } */

signed char
a (short x, short y)
{
  unsigned char t = (unsigned char) (x & y);
  unsigned char tt = (unsigned char) (x | y);
  t = t - tt;
  return (signed char) (t + -1);
}

unsigned char
a1 (signed char x, signed char y)
{
  short t = (short) (x & y);
  short tt = (short) (x | y);
  unsigned char t1 = (unsigned char) (t - tt);
  return t1 + -1;
}

signed char
b (short x, short y)
{
  unsigned char t = (unsigned char) (x & y);
  signed char tt = (signed char) (x | y);
  t = t - 1;
  return ((signed char) t - tt);
}

short
b1 (short x, short y)
{
  int t = (int) (x & y);
  int tt = (int) (x | y);
  short t1 = (short) (t - 1);
  return (short) (t1 - tt);
}

signed char
c (unsigned x, unsigned y)
{
  unsigned char t = (unsigned char) (x & y);
  signed char tt = (signed char) (x | y);
  tt = tt + 1;
  return (signed char) (t - tt);
}

unsigned char
c1 (signed char x, signed char y)
{
  unsigned char t = (unsigned char) (x & y);
  short tt = (short) (x | y);
  unsigned char tt1 = (unsigned char) (tt + 1);
  return t - tt1;
}

signed char
d (unsigned char x, unsigned char y)
{
  int t = (int) (x & y);
  int tt = (int) (x | y);
  tt = tt + 1;
  return (signed char) (t - tt);
}

unsigned char
d1 (int x, int y)
{
  signed char t = (signed char) (x & y);
  signed char tt = (signed char) (x | y);
  unsigned char tt1 = (unsigned char) (tt + 1);
  return (unsigned char) (t - tt1);
}
