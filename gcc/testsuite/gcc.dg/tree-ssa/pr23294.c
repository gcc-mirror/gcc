/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-vars" } */

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

/* { dg-final { scan-tree-dump-times "a \\\* 5" 3 "vars" } } */
/* { dg-final { scan-tree-dump "\\\(b \\\* 3 \\\+ a\\\) \\\* 2" "vars" } } */
/* { dg-final { scan-tree-dump "\\\(a - b \\\* 3\\\) \\\* 2" "vars" } } */
/* { dg-final { scan-tree-dump "\\\(a \\\* 3 - b\\\) \\\* 2" "vars" } } */
/* { dg-final { cleanup-tree-dump "vars" } } */
