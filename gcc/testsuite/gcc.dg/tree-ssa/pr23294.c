/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

int f1(int a)
{
  return a*6-a;
}

int f2(int a)
{
  return a*4+a;
}

int f3(int a)
{
  return 2*a + 3*a;
}

int f4(int a, int b)
{
  return 2*a + 6*b;
}

int f5(int a, int b)
{
  return 2*a - 6*b;
}

int f6(int a, int b)
{
  return 6*a - 2*b;
}

/* { dg-final { scan-tree-dump-times "a \\\* 5" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times "\\\) \\\* 2" 3 "optimized" } } */
/* { dg-final { scan-tree-dump-not "\\\* 6" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
