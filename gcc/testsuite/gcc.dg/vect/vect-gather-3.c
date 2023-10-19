#include "tree-vect.h"

#define N 16

void __attribute__((noipa))
f (int *restrict y, int *restrict x, int *restrict indices)
{
  for (int i = 0; i < N; ++i)
    {
      y[i * 2] = (indices[i * 2] < N * 2
		  ? x[indices[i * 2]] + 1
		  : 1);
      y[i * 2 + 1] = (indices[i * 2 + 1] < N * 2
		      ? x[indices[i * 2 + 1]] + 2
		      : 2);
    }
}

int y[N * 2];
int x[N * 2] = {
  72704, 52152, 51301, 96681,
  57937, 60490, 34504, 60944,
  42225, 28333, 88336, 74300,
  29250, 20484, 38852, 91536,
  86917, 63941, 31590, 21998,
  22419, 26974, 28668, 13968,
  3451, 20247, 44089, 85521,
  22871, 87362, 50555, 85939
};
int indices[N * 2] = {
  15, 0x10000, 0xcafe0, 19,
  7, 22, 19, 1,
  0x20000, 0x70000, 15, 30,
  5, 12, 11, 11,
  10, 25, 5, 20,
  22, 24, 32, 28,
  30, 19, 6, 0xabcdef,
  7, 12, 8, 21
};
int expected[N * 2] = {
  91537, 2, 1, 22000,
  60945, 28670, 21999, 52154,
  1, 2, 91537, 50557,
  60491, 29252, 74301, 74302,
  88337, 20249, 60491, 22421,
  28669, 3453, 1, 22873,
  50556, 22000, 34505, 2,
  60945, 29252, 42226, 26976
};

int
main (void)
{
  check_vect ();

  f (y, x, indices);
#pragma GCC novector
  for (int i = 0; i < 32; ++i)
    if (y[i] != expected[i])
      __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump "Loop contains only SLP stmts" vect { target { { vect_gather_load_ifn || avx2 } && vect_masked_load } } } } */
