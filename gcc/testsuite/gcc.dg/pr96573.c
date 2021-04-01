/* PR tree-optimization/96573 */
/* { dg-do compile { target { lp64 || ilp32 } } } */
/* { dg-require-effective-target bswap } */
/* { dg-options "-O3 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump "__builtin_bswap" "optimized" } } */

typedef __SIZE_TYPE__ size_t;

void *
foo (void * const p)
{
  const size_t m = sizeof (p) - 1;
  const unsigned char * const o = (unsigned char*) &p;
  void *n;
  unsigned char * const q = (unsigned char *) &n;
  unsigned char i;
  for (i = 0; i <= m; ++i)
    q[m - i] = o[i];
  return n;
}
