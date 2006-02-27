/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */
int test1(int a)
{
  return !(a & 4);
}

int test2(int b)
{
  return (b & 4) == 0;
}

int test3(int c)
{
  return ((c & 4) ^ 4) != 0;
}

int test4(int d)
{
  return ((d ^ 4) & 4) != 0;
}

int test5(int e)
{
  return (~e & 4) != 0;
}

/* { dg-final { scan-tree-dump-times "\\(a \& 4\\) == 0" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "\\(b \& 4\\) == 0" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "\\(c \& 4\\) == 0" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "\\(d \& 4\\) == 0" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "\\(e \& 4\\) == 0" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
