/* PR middle-end/21137  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-original" } */
extern void foo();

void test1(int a)
{
  if ((a >> 3) & 1)
    foo ();
}

void test2(int b)
{
  if ((b >> 3) & 4)
    foo ();
}

int test3(int c)
{
  return (c >> 3) & 1;
}

int test4(int d)
{
  return (d >> 3) & 4;
}

void test5(int e)
{
  if ((e >> 31) & 64)
    foo();
}

void test6(unsigned int f)
{
  if ((f >> 31) & 64)
    foo();
}

/* { dg-final { scan-tree-dump-times "\\(a \& 8\\) != 0" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "\\(b \& 32\\) != 0" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "c >> 3 \& 1" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "d >> 3 \& 4" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "e < 0" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "if \\(0\\)" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
