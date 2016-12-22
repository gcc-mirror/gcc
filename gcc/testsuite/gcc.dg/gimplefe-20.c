/* { dg-do compile } */
/* { dg-options "-O -fgimple -fdump-tree-ssa-gimple" } */

_Complex a;

double __GIMPLE() f()
{
  double t1;
  double t2;
  double _1;

bb1:
  t1_2 = __real a;
  t2_3 = __imag a;
  _1 = t1_2 + t2_3;
  return _1;
}

/* { dg-final { scan-tree-dump "__real a" "ssa" } } */
/* { dg-final { scan-tree-dump "__imag a" "ssa" } } */
