/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized-raw" } */
/* minmax (a, b) + c -> minmax (a + c, b + c) */
/* PR tree-optimization/127103 */

__attribute__((always_inline))
static inline unsigned
min(unsigned a, unsigned b)
{
  if (a > b)
    return b;
  return a;
}
__attribute__((always_inline))
static inline unsigned
max(unsigned a, unsigned b)
{
  if (a < b)
    return b;
  return a;
}

unsigned f(unsigned a, unsigned t1)
{
  if (a > __INT_MAX__)
    return 0;
  unsigned t = __INT_MAX__ - a;
  return a + min(t, a);
}

unsigned f1(unsigned a, unsigned t1)
{
  if (a > __INT_MAX__)
    return 0;
  unsigned t = __INT_MAX__ - a;
  return a + max(t, a);
}

/* { dg-final { scan-tree-dump-times "gimple_assign <mult_expr, _\[0-9\], a_\[0-9\].D., 2" 2 "optimized" } } */
