/* Gather load table[i & 4]: mask 4 affects only bit 2, so (4 & (N - 1)) == 0
   when N = 4.  -mprefer-vector-width=128 encourages 4-wide int vectors on
   x86 so i & 4 is uniform per vector chunk and broadcast is possible.  */
/* { dg-do compile { target { x86_64-*-* i?86-*-* } } } */
/* { dg-additional-options "-mprefer-vector-width=128" } */

#define N 32

int block_values[8] = {10, 20, 30, 40, 50, 60, 70, 80};
int result[N];

void
tile_fill (int *out, int *__restrict table, int n, int *in)
{
  for (int i = 0; i < n; i++)
    {
      out[i] = table[i & 4];
      in[i] = out[i] + 10;
    }
}

void
use_tile_fill (void)
{
  tile_fill (result, block_values, N, result);
  __asm__ volatile ("" : : "r" (result) : "memory");
}

/* { dg-final { scan-tree-dump "loop vectorized" "vect" } } */
/* { dg-final { scan-tree-dump "computing cost for broadcast" "vect" } } */
