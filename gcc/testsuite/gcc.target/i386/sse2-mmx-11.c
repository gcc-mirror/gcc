/* { dg-do run { target { ! ia32 } } } */
/* { dg-options "-O2 -fno-strict-aliasing -msse2 -mno-mmx" } */

#include "sse2-check.h"
#include "mmx-vals.h"

__attribute__((noinline, noclone))
static void
test_from_long_long  (long long *ll1, long long *r)
{
  *(__m64 *) r = _mm_cvtsi64_m64 (*ll1);
}

/* Routine to manually compute the results */
static void
compute_correct_result (long long *ll1, long long *r)
{
  *r = *ll1;
}

static void
sse2_test (void)
{
  int i;
  long long r, ck;
  int fail = 0;

  /* Run the MMX tests */
  for (i = 0; i < MMX_num_ops; i++)
    {
      test_from_long_long (&MMXops[i], &r);
      compute_correct_result (&MMXops[i], &ck);
      if (ck != r)
	fail++;
    }

  if (fail != 0)
    abort ();
}
