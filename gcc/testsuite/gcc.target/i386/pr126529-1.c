/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64" } */

typedef signed char A[[gnu::vector_size (2)]];
typedef short B[[gnu::vector_size (16)]];
int a, b, c, d;
B e;
[[gnu::vector_size(8 * sizeof (int))]] int f;
_Bool g;

__attribute__((noipa, noinline, target("avx2")))
void *
foo (long long x, _Bool y, int z, B w)
{
  A h = {};
  short i = h[z];
  f = 0 % f;
  w = e;
  d = w[g];
  h = ((union { short s; A t; }) { i }).t;
  b = 3 / x;
  h = __builtin_shufflevector (h, h, 3, 2);
lab:
  c = h[0];
  if (y)
    return 0;
  y = 1;
  h = ~h;
  goto lab;
}

int
main (void)
{
 if (__builtin_cpu_supports ("avx2"))
   foo (3355934481768670720LL, 0, a, e);

  return 0;
}
