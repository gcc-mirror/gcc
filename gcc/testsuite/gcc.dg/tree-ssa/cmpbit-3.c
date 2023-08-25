/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-raw -fdump-tree-dse1-raw -fdump-tree-forwprop1" } */

_Bool f(int a, int b)
{
  _Bool X = a==1, Y = b == 2;
return (X & !Y) | (!X & Y);
}


_Bool f1(int a, int b)
{
  _Bool X = a==1, Y = b == 2;
  _Bool c = (X & !Y);
  _Bool d = (!X & Y);
  return c | d;
}

/* Both of these should be optimized to (a==1) ^ (b==2) or (a != 1) ^ (b != 2) */
/* { dg-final { scan-tree-dump-not "gimple_cond " "optimized" } } */
/* { dg-final { scan-tree-dump-not "gimple_phi " "optimized" } } */
/* { dg-final { scan-tree-dump-times "ne_expr|eq_expr, " 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times "bit_xor_expr, " 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "gimple_assign " 6 "optimized" } } */

/* Both of these should be optimized early in the pipeline after forwprop1 */
/* { dg-final { scan-tree-dump-times "ne_expr|eq_expr, " 4 "forwprop1" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "bit_xor_expr, " 2 "forwprop1" { xfail *-*-* } } } */
/* { dg-final { scan-tree-dump-times "gimple_assign " 6 "forwprop1" { xfail *-*-* } } } */
/* Note forwprop1 does not remove all unused statements sometimes so test dse1 also. */
/* { dg-final { scan-tree-dump-times "ne_expr|eq_expr, " 4 "dse1" } } */
/* { dg-final { scan-tree-dump-times "bit_xor_expr, " 2 "dse1" } } */
/* { dg-final { scan-tree-dump-times "gimple_assign " 6 "dse1" } } */
