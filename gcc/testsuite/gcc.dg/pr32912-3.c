/* { dg-do compile } */
/* { dg-options "-w -O2 -fdump-tree-optimized" } */

typedef int __m128i __attribute__ ((__vector_size__ (16)));

__m128i
bar (void)
{
  __m128i x = { 0x11111111, 0x22222222, 0x44444444 };
  return ~x;
}

/* { dg-final { scan-tree-dump-not "~\{" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
