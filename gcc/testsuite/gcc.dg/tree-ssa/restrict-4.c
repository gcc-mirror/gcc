/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized -fno-ipa-icf" } */

int
foo (int *x, int y)
{
  int *__restrict p1 = x;
  int *__restrict p2 = x + 32;
  p1[y] = 1;
  p2[4] = 2;
  return p1[y];
}

int
bar (int *x, int y)
{
  int *__restrict p1 = x;
  int *p3 = x + 32;
  int *__restrict p2 = p3;
  p1[y] = 1;
  p2[4] = 2;
  return p1[y];
}

/* { dg-final { scan-tree-dump-times "return 1;" 2 "optimized" { xfail *-*-* } } } */
