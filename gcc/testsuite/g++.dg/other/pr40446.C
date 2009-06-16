// PR middle-end/40446
// { dg-do run { target i?86-*-* x86_64-*-* } }
// { dg-options "-O1 -msse2" }

#include <emmintrin.h>
#include "cpuid.h"

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
  unsigned int eax, ebx, ecx, edx;

  if (!__get_cpuid (1, &eax, &ebx, &ecx, &edx))
    return 0;

  /* Run SSE2 test only if host has SSE2 support.  */
  if (edx & bit_SSE2)
    sse2_test ();

  return 0;
}
