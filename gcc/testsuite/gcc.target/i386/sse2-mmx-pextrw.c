/* { dg-do run } */
/* { dg-options "-O2 -fno-strict-aliasing -msse2" } */
/* { dg-additional-options "-mno-mmx" { target { ! ia32 } } } */

#include <string.h>
#include "sse2-check.h"

__attribute__((noinline, noclone))
static void
test_pextrw (__m64 *i, unsigned int imm, int *r)
{
  switch (imm)
    {
    case 0:
      *r = _m_pextrw (*i, 0);
      break;
    case 1:
      *r = _m_pextrw (*i, 1);
      break;
    case 2:
      *r = _m_pextrw (*i, 2);
      break;
    case 3:
      *r = _m_pextrw (*i, 3);
      break;
    default:
      break;
    }
}

/* Routine to manually compute the results */
static void
compute_correct_result (__m64 *src_p, unsigned int imm, int *res_p)
{
  unsigned short *src = (unsigned short *) src_p;
  if (imm < 4)
    *res_p = src[imm];
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
      test_pextrw ((__m64 *) &y, i, &r);
      compute_correct_result ((__m64 *) &y, i, &ck);
      if (r != ck)
	failed++;
    }

  if (failed)
    abort ();
}
