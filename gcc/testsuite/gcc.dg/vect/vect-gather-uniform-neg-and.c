/* Negative test: the offset must NOT be reported as uniform for ANY candidate
   vector size the vectorizer tries.

   NOTE: a right-shift offset (i >> k) cannot be used for a negative test here.
   The vectorizer analyzes every candidate vector size (32, 16, 8, 4, 2), and
   for a right shift the offset is uniform per chunk iff N - 1 < 2^k.  At the
   smallest candidate N = 2 this is 1 < 2^k, which holds for every k >= 1, so
   *some* candidate always reports "uniform" and scan-tree-dump-not would fail.

   For BIT_AND the offset is uniform iff (k & (N - 1)) == 0.  With an odd mask
   (k = 1) bit 0 is set, and N - 1 has bit 0 set for every N >= 2, so the
   offset is non-uniform for ALL candidate vector sizes.  The broadcast
   optimization must never trigger.  */
/* { dg-do compile { target { x86_64-*-* i?86-*-* } } } */

#define N 512

int block_values[64];
int result[N];

void
tile_fill (int *__restrict out, int *__restrict table, int n)
{
  for (int i = 0; i < n; i++)
    out[i] = table[i & 1];
}

void
use_tile_fill (void)
{
  tile_fill (result, block_values, N);
  __asm__ volatile ("" : : "r" (result) : "memory");
}

/* { dg-final { scan-tree-dump "loop vectorized" "vect" } } */
/* { dg-final { scan-tree-dump-not "computing cost for broadcast" "vect" } } */
