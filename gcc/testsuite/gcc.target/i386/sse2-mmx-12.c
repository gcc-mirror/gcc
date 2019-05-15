/* { dg-do run } */
/* { dg-options "-O2 -fno-strict-aliasing -msse2" } */
/* { dg-additional-options "-mno-mmx" { target { ! ia32 } } } */

#include "sse2-check.h"
#include "mmx-vals.h"

__attribute__((noinline, noclone))
static void
test_to_int  (long long *ll1, long long *r)
{
  __m64 m = *(__m64 *) ll1; 
  *(int *) r = _m_to_int (m);
}

/* Routine to manually compute the results */
static void
compute_correct_result (long long *ll1, long long *r)
{
  int *i1 = (int *) ll1;
  *(int *) r = *i1;
}

static void
sse2_test (void)
{
  int i;
  long long r = 0, ck = 0;
  int fail = 0;

  /* Run the MMX tests */
  for (i = 0; i < MMX_num_ops; i++)
    {
      test_to_int (&MMXops[i], &r);
      compute_correct_result (&MMXops[i], &ck);
      if (ck != r)
	fail++;
    }

  if (fail != 0)
    abort ();
}
