/* { dg-do compile { target *-*-mingw* *-*-cygwin* } } */
/* { dg-require-effective-target ia32 } */
/* { dg-options "-msse -O" } */

extern void abort (void);

typedef float __m128 __attribute__ ((__vector_size__ (16), __may_alias__));

static __m128
load_m128 (float *e)
{
  return * (__m128 *) e;
}

typedef union
{
  __m128  x;
  float a[4];
} union128;

void test (void)
{
  union128 u;
  float e[4] __attribute__ ((aligned (16)))
    = {2134.3343, 1234.635654, 1.2234, 876.8976};
  int i;

  u.x = load_m128 (e);

  for (i = 0; i < 4; i++)
    if (u.a[i] != e[i])
      abort ();
}

/* { dg-final { scan-assembler "andl\\t\\$-16, %esp" } } */
