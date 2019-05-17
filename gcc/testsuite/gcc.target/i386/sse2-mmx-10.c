/* { dg-do run } */
/* { dg-options "-O2 -fno-strict-aliasing -msse2" } */
/* { dg-additional-options "-mno-mmx" { target { ! ia32 } } } */

#include "sse2-check.h"
#include "mmx-vals.h"

__attribute__((noinline, noclone))
static void
test_from_int  (long long *ll1, long long *r)
{
  int i1 = *(int *) ll1;
  *(__m64 *) r = _m_from_int (i1);
}

/* Routine to manually compute the results */
static void
compute_correct_result (long long *ll1, long long *r)
{
  int *res = (int *) r;
  res[0] = *(int *) ll1;
  res[1] = 0;
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
      test_from_int (&MMXops[i], &r);
      compute_correct_result (&MMXops[i], &ck);
      if (ck != r)
	fail++;
    }

  if (fail != 0)
    abort ();
}
