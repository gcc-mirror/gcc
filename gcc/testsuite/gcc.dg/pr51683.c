/* PR tree-optimization/51683 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

static inline void *
bar (void *p, void *q, int r)
{
  return __builtin_memcpy (p, q, r);
}

void *
foo (void *p)
{
  return bar ((void *) 0x12345000, p, 256);
}

/* { dg-final { scan-tree-dump "memcpy" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
