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
foo (float f1, float f2, float f3, float f4, float f5, float f6, float f7,
     float f8, long long x, _Bool y, int z, B w)
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
   foo (1, 2, 3, 4, 5, 6, 7, 8, 3355934481768670720LL, 0, a, e);

  return 0;
}
