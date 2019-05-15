/* { dg-do run } */
/* { dg-options "-O2 -fno-strict-aliasing -msse2" } */
/* { dg-additional-options "-mno-mmx" { target { ! ia32 } } } */

#include "sse2-check.h"

__attribute__((noinline, noclone))
static void
test_setzero (long long *r)
{
  *(__m64 *) r = _mm_setzero_si64 ();
}

/* Routine to manually compute the results */
static void
compute_correct_result (long long *r)
{
  *r = 0x0LL;
}

static void
sse2_test (void)
{
  long long r, ck;

  /* Run the MMX tests */
  test_setzero (&r);
  compute_correct_result (&ck);
  if (ck != r)
    abort ();
}
