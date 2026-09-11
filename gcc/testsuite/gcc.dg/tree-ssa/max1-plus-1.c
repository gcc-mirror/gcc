/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */
/* x + uMax(x, 1) -> uMax(x+x, 1) */
/* PR tree-optimization/127103 */

__attribute__((always_inline))
static inline unsigned
max(unsigned a, unsigned b)
{
  if (a < b)
    return b;
  return a;
}

unsigned f1(unsigned a, unsigned t1)
{
  if (a > __INT_MAX__)
    return 0;
  return a + max(1, a);
}

/* { dg-final { scan-tree-dump "gimple_assign <mult_expr, _\[0-9\], a_\[0-9\].D., 2" "optimized" } } */
