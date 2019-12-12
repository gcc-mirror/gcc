/* { dg-do run } */
/* { dg-options "-O2 -fno-strict-aliasing -msse2" } */
/* { dg-additional-options "-mno-mmx" { target { ! ia32 } } } */

#include <string.h>
#include "sse2-check.h"

__attribute__((noinline, noclone))
static void
test_pinsrw  (__m64 *i, int val, unsigned int imm, int *r)
{
  switch (imm)
    {
    case 0:
       *(__m64 *) r = _m_pinsrw  (*i, val, 0);
      break;
    case 1:
       *(__m64 *) r = _m_pinsrw  (*i, val, 1);
      break;
    case 2:
       *(__m64 *) r = _m_pinsrw  (*i, val, 2);
      break;
    case 3:
       *(__m64 *) r = _m_pinsrw  (*i, val, 3);
      break;
    default:
      break;
    }
}

/* Routine to manually compute the results */
static void
compute_correct_result (__m64 *src_p, int val, unsigned int imm,
			int *res_p)
{
  short *res = (short *) res_p;
  *(__m64 *) res_p = *src_p;
  if (imm < 4)
    res[imm] = val;
}

static void
sse2_test (void)
{
  int r, ck;
  int i;
  int failed = 0;
  __v4hi y = { 3320, -3339, 48, 4392 };

  /* Run the MMX tests */
  for (i = 0; i < 4; i++)
    {
      test_pinsrw  ((__m64 *) &y, 0x1234, i, &r);
      compute_correct_result ((__m64 *) &y, 0x1234, i, &ck);
      if (r != ck)
	failed++;
    }

  if (failed)
    abort ();
}
