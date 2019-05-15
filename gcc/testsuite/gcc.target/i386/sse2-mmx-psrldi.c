/* { dg-do run } */
/* { dg-options "-O2 -fno-strict-aliasing -msse2" } */
/* { dg-additional-options "-mno-mmx" { target { ! ia32 } } } */

#include "sse2-check.h"
#include "mmx-vals.h"

__attribute__((noinline, noclone))
static void
test_psrldi  (long long *ll1, unsigned int imm, long long *r)
{
  __m64 t1 = *(__m64 *) ll1;
  switch (imm)
    {
    case 0:
      *(__m64 *) r = _m_psrldi (t1, 0);
      break;
    case 1:
      *(__m64 *) r = _m_psrldi (t1, 1);
      break;
    case 2:
      *(__m64 *) r = _m_psrldi (t1, 2);
      break;
    case 3:
      *(__m64 *) r = _m_psrldi (t1, 3);
      break;
    case 4:
      *(__m64 *) r = _m_psrldi (t1, 4);
      break;
    case 5:
      *(__m64 *) r = _m_psrldi (t1, 5);
      break;
    case 6:
      *(__m64 *) r = _m_psrldi (t1, 6);
      break;
    case 7:
      *(__m64 *) r = _m_psrldi (t1, 7);
      break;
    case 8:
      *(__m64 *) r = _m_psrldi (t1, 8);
      break;
    case 9:
      *(__m64 *) r = _m_psrldi (t1, 9);
      break;
    case 10:
      *(__m64 *) r = _m_psrldi (t1, 10);
      break;
    case 11:
      *(__m64 *) r = _m_psrldi (t1, 11);
      break;
    case 12:
      *(__m64 *) r = _m_psrldi (t1, 12);
      break;
    case 13:
      *(__m64 *) r = _m_psrldi (t1, 13);
      break;
    case 14:
      *(__m64 *) r = _m_psrldi (t1, 14);
      break;
    case 15:
      *(__m64 *) r = _m_psrldi (t1, 15);
      break;
    case 16:
      *(__m64 *) r = _m_psrldi (t1, 16);
      break;
    case 17:
      *(__m64 *) r = _m_psrldi (t1, 17);
      break;
    case 18:
      *(__m64 *) r = _m_psrldi (t1, 18);
      break;
    case 19:
      *(__m64 *) r = _m_psrldi (t1, 19);
      break;
    case 20:
      *(__m64 *) r = _m_psrldi (t1, 20);
      break;
    case 21:
      *(__m64 *) r = _m_psrldi (t1, 21);
      break;
    case 22:
      *(__m64 *) r = _m_psrldi (t1, 22);
      break;
    case 23:
      *(__m64 *) r = _m_psrldi (t1, 23);
      break;
    case 24:
      *(__m64 *) r = _m_psrldi (t1, 24);
      break;
    case 25:
      *(__m64 *) r = _m_psrldi (t1, 25);
      break;
    case 26:
      *(__m64 *) r = _m_psrldi (t1, 26);
      break;
    case 27:
      *(__m64 *) r = _m_psrldi (t1, 27);
      break;
    case 28:
      *(__m64 *) r = _m_psrldi (t1, 28);
      break;
    case 29:
      *(__m64 *) r = _m_psrldi (t1, 29);
      break;
    case 30:
      *(__m64 *) r = _m_psrldi (t1, 30);
      break;
    case 31:
      *(__m64 *) r = _m_psrldi (t1, 31);
      break;
    default:
      *(__m64 *) r = _m_psrldi (t1, 32);
      break;
    }
}

/* Routine to manually compute the results */
static void
compute_correct_result (long long *src_p, unsigned int imm,
			long long *res_p)
{
  int *src = (int *) src_p;
  int *res = (int *) res_p;
  int i;
  if (imm > 31)
    for (i = 0; i < 2; i++)
      res[i] = 0;
  else
    for (i = 0; i < 2; i++)
      res[i] = src[i] >> imm;
}

static void
sse2_test (void)
{
  int i;
  unsigned int count;
  long long r, ck;
  int fail = 0;

  /* Run the MMX tests */
  for (i = 0; i < MMX_num_ops; i++)
    {
      count = MMXops[i];
      test_psrldi (&MMXops[i], count, &r);
      compute_correct_result (&MMXops[i], count, &ck);
      if (ck != r)
	  fail++;
      }

  if (fail != 0)
    abort ();
}
