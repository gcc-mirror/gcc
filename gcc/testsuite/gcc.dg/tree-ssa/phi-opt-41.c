/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-phiopt1" } */
/* PR tree-optimization/112392 */

int feq_1(int a, unsigned char b)
{
  int absb = b;
  if (a == absb)  return absb;
  return a > 0 ? a : -a;
}
int feq_2(int a, unsigned char b)
{
  int absb = b;
  if (a == absb)  return a;
  return a > 0 ? a : -a;
}

int fgt(int a, unsigned char b)
{
  int absb = b;
  if (a > absb)  return a;
  return a > 0 ? a : -a;
}

int fge(int a, unsigned char b)
{
  int absb = b;
  if (a >= absb)  return a;
  return a > 0 ? a : -a;
}

/* The ABS_EXPR gets rewritten to ABSU_EXPR as phiopt can't prove it was not undefined when moving it. */
/* { dg-final { scan-tree-dump-not "if " "phiopt1" } } */
/* { dg-final { scan-tree-dump-times "ABSU_EXPR <" 4 "phiopt1" } } */
