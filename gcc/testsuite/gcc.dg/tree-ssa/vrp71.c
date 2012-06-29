/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

int foo(int *p)
{
  int x = -10;
  if (p[0]) x++;
  if (p[1]) x++;
  if (p[2]) x++;
  if (p[3]) x++;
  x <<= 2;
  return (x > 0);
}

int bar(char c)
{
  int i = c << 1;
  return i > 1000;
}

/* { dg-final { scan-tree-dump-times "return 0;" 2 "vrp1" } } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
