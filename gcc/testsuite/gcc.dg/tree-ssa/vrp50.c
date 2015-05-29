/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

int
foo (unsigned int i, unsigned int j)
{
  i &= 15;
  j &= 15;
  i += 1024;
  j += 2048;
  i &= j;
  return i < 16;
}

int
bar (int i)
{
  int c = 2;
  c &= i > 6;
  return c == 0;
}

int baz (int x, int y)
{
  x &= 15;
  y &= 15;
  x += 4;
  y += 16;
  x &= y;
  return x < 20;
}

/* { dg-final { scan-tree-dump-times "return 1;" 3 "vrp1" } } */
