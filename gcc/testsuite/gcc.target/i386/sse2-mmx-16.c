/* { dg-do run } */
/* { dg-options "-O2 -fno-strict-aliasing -msse2" } */
/* { dg-additional-options "-mno-mmx" { target { ! ia32 } } } */

#include "sse2-check.h"

__attribute__((noinline, noclone))
static void
test_set (int i0, int i1, int i2, int i3, long long *r)
{
  *(__m64 *) r = _mm_set_pi16 (i0, i1, i2, i3);
}

/* Routine to manually compute the results */
static void
compute_correct_result (int i0, int i1, int i2, int i3, long long *res_p)
{
  short *res = (short *) res_p;
  res[0] = i3;
  res[1] = i2;
  res[2] = i1;
  res[3] = i0;
}

static void
sse2_test (void)
{
  short i0, i1, i2, i3;
  long long r, ck;

  /* Run the MMX tests */
  i0 = 0x0bad;
  i1 = 0xbeef;
  i2 = 0x0bad;
  i3 = 0xfeed;
  test_set (i0, i1, i2, i3, &r);
  compute_correct_result (i0, i1, i2, i3, &ck);
  if (ck != r)
    abort ();
}
