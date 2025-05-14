/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-phiopt" } */
/* PR tree-optimization/111957 */

int f(int a)
{
  if (a)
    return a > 0 ? a : -a;
  return 0;
}

int f1(int x)
{
  int intmin = (-1u >> 1);
  intmin = -intmin - 1;
  if (x != intmin)
    return x > 0 ? x : -x;
  return intmin;
}

/* { dg-final { scan-tree-dump-times "if " 1 "phiopt1" } } */
/* { dg-final { scan-tree-dump-not "if " "phiopt2" } } */
/* The ABS_EXPR in f gets rewritten to ABSU_EXPR as phiopt can't prove it was not undefined when moving it. */
/* { dg-final { scan-tree-dump-times "ABS_EXPR <" 1 "phiopt1" } } */
/* { dg-final { scan-tree-dump-times "ABSU_EXPR <" 1 "phiopt1" } } */
/* { dg-final { scan-tree-dump-times "ABSU_EXPR <" 2 "phiopt2" } } */
