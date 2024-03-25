/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99 -O3 -ftree-vectorize -fno-vect-cost-model -ffast-math" } */

#define N 4
struct C { int l, r; };
struct C a[N], b[N], c[N];
struct C a1[N], b1[N], c1[N];

void __attribute__((noinline))
init_data_vec (struct C * __restrict a, struct C * __restrict b,
	       struct C * __restrict c)
{
  int i;

  for (i = 0; i < N; ++i)
    {
      a[i].l = N - i;
      a[i].r = i - N;

      b[i].l = i - N;
      b[i].r = i + N;

      c[i].l = -1 - i;
      c[i].r = 2 * N - 1 - i;
    }
}

int
main ()
{
  int i;

  init_data_vec (a, b, c);

#pragma GCC novector
  for (i = 0; i < N; ++i)
    {
      a1[i].l = N - i;
      a1[i].r = i - N;

      b1[i].l = i - N;
      b1[i].r = i + N;

      c1[i].l = -1 - i;
      c1[i].r = 2 * N - 1 - i;
    }

  for (i = 0; i < N; i++)
    {
      if (a[i].l != a1[i].l || a[i].r != a1[i].r)
	__builtin_abort ();

      if (b[i].l != b1[i].l || b[i].r != b1[i].r)
	__builtin_abort ();

      if (c[i].l != c1[i].l || c[i].r != c1[i].r)
	__builtin_abort ();
    }

  return 0;
}
