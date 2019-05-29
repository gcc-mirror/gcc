/* { dg-do run } */
/* { dg-options "-O2 -fno-strict-aliasing -msse2" } */
/* { dg-additional-options "-mno-mmx" { target { ! ia32 } } } */

#include "sse2-check.h"
#include "mmx-vals.h"

__attribute__((noinline, noclone))
static void
test_pmovmskb  (long long *ll1, int *r)
{
  __m64 t1 = *(__m64 *) ll1;
  *r = _m_pmovmskb (t1);
}

/* Routine to manually compute the results */
static void
compute_correct_result (long long *src_p, int *res_p)
{
  char *src = (char *) src_p;
  int i;
  int res = 0;
  for (i = 0; i < 8; i++)
    res |= ((src[i] & 0x80) >> 7) << i;
  *res_p = res;
}

static void
sse2_test (void)
{
  int i;
  int r, ck;
  int fail = 0;

  /* Run the MMX tests */
  for (i = 0; i < MMX_num_ops; i++)
    {
      test_pmovmskb (&MMXops[i], &r);
      compute_correct_result (&MMXops[i], &ck);
      if (ck != r)
	fail++;
    }

  if (fail != 0)
    abort ();
}
