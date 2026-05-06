/* The store to out[i + 1] is misaligned, so the vectorizer peels for alignment
   by a known amount (3 for 4 x int).  The vector loop then starts at i = 3 and
   the first chunk {3,4,5,6} gives i >> 2 = {0,1,1,1}, which is not uniform.  */

#include "tree-vect.h"

#define N 1024

int table[N];
int out[N + 16];

void __attribute__((noipa))
f (int n)
{
  for (int i = 0; i < n; i++)
    out[i + 1] = table[i >> 2];
}

int
main (void)
{
  check_vect ();

  for (int i = 0; i < N; i++)
    table[i] = i;

  f (N - 8);

#pragma GCC novector
  for (int i = 0; i < N - 8; i++)
    if (out[i + 1] != table[i >> 2])
      __builtin_abort ();

  return 0;
}
