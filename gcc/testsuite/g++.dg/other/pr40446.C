// PR middle-end/40446
// { dg-do run { target i?86-*-* x86_64-*-* } }
// { dg-options "-O1 -msse2" }
// { dg-require-effective-target sse2_runtime }
/* { dg-skip-if "requires hosted libstdc++ for cstdlib malloc" { ! hostedlib } } */

#include <emmintrin.h>

extern "C" void abort ();

struct S
{
  S (double r, double i) { __real__ s = r; __imag__ s = i; }
  __complex__ double s;
};

__m128d
foo ()
{
  S c (0, 1);
  return _mm_load_pd ((double *) &c);
}

static void
__attribute__((noinline))
sse2_test ()
{
  union { __m128d vec; double val[2]; } u;
  u.vec = foo ();
  if (u.val[0] != 0 || u.val[1] != 1)
    abort ();
}

int
main ()
{
  sse2_test ();
  return 0;
}
