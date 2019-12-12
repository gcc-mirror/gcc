/* { dg-do run } */
/* { dg-options "-O2 -fno-strict-aliasing -msse2" } */
/* { dg-additional-options "-mno-mmx" { target { ! ia32 } } } */

#include "sse2-check.h"
#include "mmx-vals.h"

__attribute__((noinline, noclone))
static void
test_pslld  (long long *ll1, long long *ll2, long long *r)
{
  __m64 t1 = *(__m64 *) ll1;
  __m64 t2 = *(__m64 *) ll2;
  *(__m64 *) r = _m_pslld (t1, t2);
}

/* Routine to manually compute the results */
static void
compute_correct_result (long long *dst_p, long long *src_p,
			long long *res_p)
{
  unsigned int *dst = (unsigned int *) dst_p;
  unsigned int *src = (unsigned int *) src_p;
  unsigned int *res = (unsigned int *) res_p;
  int i;
  if (src[1] || src[0] > 31)
    for (i = 0; i < 2; i++)
      res[i] = 0;
  else
    for (i = 0; i < 2; i++)
      res[i] = dst[i] << src[0];
}

static void
sse2_test (void)
{
  int i;
  long long r, ck;
  int fail = 0;

  /* Run the MMX tests */
  for (i = 0; i < MMX_num_ops; i += 2)
    {
      test_pslld (&MMXops[i], &MMXops[i + 1], &r);
      compute_correct_result (&MMXops[i], &MMXops[i + 1], &ck);
      if (ck != r)
	fail++;
    }

  if (fail != 0)
    abort ();
}
