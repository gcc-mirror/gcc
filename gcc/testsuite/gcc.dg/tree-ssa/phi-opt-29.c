/* PR tree-optimization/89263 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */

_Bool f0(_Bool a, _Bool b)
{
  return a ? b : 0;
}
_Bool f1(_Bool a, _Bool b)
{
  return a ? b : 1;
}
_Bool t0(_Bool a, _Bool b)
{
  return a ? 0 : b;
}
_Bool t1(_Bool a, _Bool b)
{
  return a ? 1 : b;
}

/* { dg-final { scan-tree-dump-not "gimple_cond " "optimized" } } */
/* { dg-final { scan-tree-dump-times "bit_and_expr," 2 "optimized"  } } */
/* { dg-final { scan-tree-dump-times "bit_ior_expr," 2 "optimized"  } } */
/* { dg-final { scan-tree-dump-times "bit_not_expr," 2 "optimized"  } } */
