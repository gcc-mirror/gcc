/* { dg-do run { target *-*-linux* *-*-gnu* } } */
/* { dg-options "-O2 -mavx512er" } */
/* { dg-require-effective-target avx512er } */

#include "avx512er-check.h"

#include <x86intrin.h>
#include <unistd.h>
#include <signal.h>

static int counter;

void handler (int i)
{
  exit (0);
}

static void
avx512er_test (void)
{
  struct sigaction s;
  sigemptyset (&s.sa_mask);
  s.sa_handler = handler;
  s.sa_flags = 0;
  sigaction (SIGFPE, &s, NULL);

  __m512 a = _mm512_set1_ps (-1.f);

  _mm_setcsr ( _MM_MASK_MASK & ~_MM_MASK_INVALID );
  __m512 r1 = _mm512_rsqrt28_round_ps (a, _MM_FROUND_NO_EXC);
  __m512 r2 = _mm512_rsqrt28_round_ps (a, _MM_FROUND_CUR_DIRECTION);
  
  if (r1[0] + r2[0])
    abort ();
}
