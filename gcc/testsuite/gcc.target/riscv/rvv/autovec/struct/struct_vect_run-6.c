/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -mrvv-vector-bits=scalable -fno-vect-cost-model" } */

#include "struct_vect-6.c"

#ifndef N
#define N 93
#endif

TYPE a[N], b[N], c[N], d[N], a2[N], b2[N], c2[N], d2[N], e[N * 8];

void __attribute__ ((noinline, noclone))
init_array (TYPE *array, int n, TYPE base, TYPE step)
{
  for (int i = 0; i < n; ++i)
    array[i] = base + step * i;
}

void __attribute__ ((noinline, noclone))
check_array (TYPE *array, int n, TYPE base, TYPE step)
{
  for (int i = 0; i < n; ++i)
    if (array[i] != (TYPE) (base + step * i))
      __builtin_abort ();
}

int __attribute__ ((optimize (1)))
main (void)
{
  init_array (e, 2 * N, 11, 5);
  f2 (a, b, e, N);
  check_array (a, N, 11, 10);
  check_array (b, N, 16, 10);

  init_array (e, 3 * N, 7, 6);
  f3 (a, b, c, e, N);
  check_array (a, N, 7, 18);
  check_array (b, N, 13, 18);
  check_array (c, N, 19, 18);

  init_array (e, 4 * N, 4, 11);
  f4 (a, b, c, d, e, N);
  check_array (a, N, 4, 44);
  check_array (b, N, 15, 44);
  check_array (c, N, 26, 44);
  check_array (d, N, 37, 44);

  init_array (e, 5 * N, 3, 9);
  f5 (a, b, c, d, a2, e, N);
  check_array (a, N, 3, 45);
  check_array (b, N, 12, 45);
  check_array (c, N, 21, 45);
  check_array (d, N, 30, 45);
  check_array (a2, N, 39, 45);

  init_array (e, 6 * N, 5, 5);
  f6 (a, b, c, d, a2, b2, e, N);
  check_array (a, N, 5, 30);
  check_array (b, N, 10, 30);
  check_array (c, N, 15, 30);
  check_array (d, N, 20, 30);
  check_array (a2, N, 25, 30);
  check_array (b2, N, 30, 30);

  init_array (e, 7 * N, 7, 3);
  f7 (a, b, c, d, a2, b2, c2, e, N);
  check_array (a, N, 7, 21);
  check_array (b, N, 10, 21);
  check_array (c, N, 13, 21);
  check_array (d, N, 16, 21);
  check_array (a2, N, 19, 21);
  check_array (b2, N, 22, 21);
  check_array (c2, N, 25, 21);

  init_array (e, 8 * N, 5, 8);
  f8 (a, b, c, d, a2, b2, c2, d2, e, N);
  check_array (a, N, 5, 64);
  check_array (b, N, 13, 64);
  check_array (c, N, 21, 64);
  check_array (d, N, 29, 64);
  check_array (a2, N, 37, 64);
  check_array (b2, N, 45, 64);
  check_array (c2, N, 53, 64);
  check_array (d2, N, 61, 64);

  init_array (a, N, 2, 8);
  init_array (b, N, 6, 8);
  g2 (a, b, e, N);
  check_array (e, 2 * N, 2, 4);

  init_array (a, N, 4, 15);
  init_array (b, N, 9, 15);
  init_array (c, N, 14, 15);
  g3 (a, b, c, e, N);
  check_array (e, 3 * N, 4, 5);

  init_array (a, N, 14, 36);
  init_array (b, N, 23, 36);
  init_array (c, N, 32, 36);
  init_array (d, N, 41, 36);
  g4 (a, b, c, d, e, N);
  check_array (e, 4 * N, 14, 9);

  init_array (a, N, 3, 45);
  init_array (b, N, 12, 45);
  init_array (c, N, 21, 45);
  init_array (d, N, 30, 45);
  init_array (a2, N, 39, 45);
  g5 (a, b, c, d, a2, e, N);
  check_array (e, 5 * N, 3, 9);

  init_array (a, N, 5, 30);
  init_array (b, N, 10, 30);
  init_array (c, N, 15, 30);
  init_array (d, N, 20, 30);
  init_array (a2, N, 25, 30);
  init_array (b2, N, 30, 30);
  g6 (a, b, c, d, a2, b2, e, N);
  check_array (e, 6 * N, 5, 5);

  init_array (a, N, 7, 21);
  init_array (b, N, 10, 21);
  init_array (c, N, 13, 21);
  init_array (d, N, 16, 21);
  init_array (a2, N, 19, 21);
  init_array (b2, N, 22, 21);
  init_array (c2, N, 25, 21);
  g7 (a, b, c, d, a2, b2, c2, e, N);
  check_array (e, 7 * N, 7, 3);

  init_array (a, N, 5, 64);
  init_array (b, N, 13, 64);
  init_array (c, N, 21, 64);
  init_array (d, N, 29, 64);
  init_array (a2, N, 37, 64);
  init_array (b2, N, 45, 64);
  init_array (c2, N, 53, 64);
  init_array (d2, N, 61, 64);
  g8 (a, b, c, d, a2, b2, c2, d2, e, N);
  check_array (e, 8 * N, 5, 8);

  return 0;
}
