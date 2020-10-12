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
  /* Integers converted to pointers are assumed to be the result of
     (invalid) arithmetic on null pointers.
     { dg-prune-output "writing 256 bytes into a region of size 0" } */
}

/* { dg-final { scan-tree-dump "memcpy" "optimized" } } */
