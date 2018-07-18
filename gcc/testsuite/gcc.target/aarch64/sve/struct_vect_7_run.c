/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-options "-O2 -ftree-vectorize --save-temps" } */

#include "struct_vect_7.c"

#define N 93

TYPE a[N], b[N], c[N], d[N], e[N * 4];

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

  return 0;
}
