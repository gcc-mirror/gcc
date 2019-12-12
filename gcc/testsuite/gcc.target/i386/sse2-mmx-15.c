/* { dg-do run } */
/* { dg-options "-O2 -fno-strict-aliasing -msse2" } */
/* { dg-additional-options "-mno-mmx" { target { ! ia32 } } } */

#include "sse2-check.h"

__attribute__((noinline, noclone))
static void
test_set (int x, int y, long long *r)
{
  *(__m64 *) r = _mm_set_pi32 (x, y);
}

/* Routine to manually compute the results */
static void
compute_correct_result (int x, int y, long long *res_p)
{
  int *res = (int *) res_p;
  res[0] = y;
  res[1] = x;
}

static void
sse2_test (void)
{
  int x, y;
  long long r, ck;

  /* Run the MMX tests */
  x = 0x0badbeef;
  y = 0x0badfeed;
  test_set (x, y, &r);
  compute_correct_result (x, y, &ck);
  if (ck != r)
    abort ();
}
