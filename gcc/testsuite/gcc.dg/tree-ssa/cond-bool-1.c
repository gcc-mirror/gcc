/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */
_Bool f1(int a, int b)
{
  _Bool _1 = b != 0;
  _Bool _2 = a != 0;
  _Bool _8 = a == 0;
  _Bool _13;
  if (_1) _13 = _8; else _13 = _2;
  return _13;
}

/* We should be able to optimize this to (a != 0) ^ (b != 0) */
/* There should be no negate_expr nor gimple_cond here. */

/* { dg-final { scan-tree-dump-not "negate_expr, " "optimized" } } */
/* { dg-final { scan-tree-dump-times "ne_expr, " 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not "gimple_cond " "optimized" } } */
/* { dg-final { scan-tree-dump-not "gimple_phi " "optimized" } } */
/* { dg-final { scan-tree-dump-times "bit_xor_expr, " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "gimple_assign " 3 "optimized" } } */
