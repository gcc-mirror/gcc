/* { dg-do run } */
/* { dg-options "-O2 -mavx512f -std=gnu99" } */
/* { dg-require-effective-target avx512f } */
/* { dg-require-effective-target c99_runtime } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"
#include "math_m_pi.h"
#include "float.h"


static void
CALC (double *r, double dest, double src, long long tbl)
{
  switch (tbl & 0xf)
    {
    case 0:
      *r = dest;
      break;
    case 1:
      *r = src;
      break;
    case 2:
      *r = signbit (src) ? -NAN : NAN;
      break;
    case 3:
      *r = -NAN;
      break;
    case 4:
      *r = -INFINITY;
      break;
    case 5:
      *r = INFINITY;
      break;
    case 6:
      *r = signbit (src) ? -INFINITY : INFINITY;
      break;
    case 7:
      *r = 1.0 / -INFINITY;
      break;
    case 8:
      *r = 0.0;
      break;
    case 9:
      *r = -1.0;
      break;
    case 10:
      *r = 1.0;
      break;
    case 11:
      *r = 1.0 / 2.0;
      break;
    case 12:
      *r = 90.0;
      break;
    case 13:
      *r = M_PI_2;
      break;
    case 14:
      *r = DBL_MAX;
      break;
    case 15:
      *r = -DBL_MAX;
      break;
    default:
      abort ();
    }
}

void
TEST (void)
{
  int i, j;
  UNION_TYPE (AVX512F_LEN, d) res1, res2, res3, s1;
  UNION_TYPE (AVX512F_LEN, i_q) s2;
  double res_ref[SIZE];


  float vals[2] = { -10, 10 };
  int controls[8] = {0, 0x11111111, 0x77777777, 0x88888888,
    0x99999999, 0xaaaaaaaa, 0xbbbbbbbb, 0xcccccccc};

  MASK_TYPE mask = MASK_VALUE;

  for (i = 0; i < 2; i++)
    {
      for (j = 0; j < SIZE; j++)
	{
	  s1.a[j] = vals[i];
	  s2.a[j] = controls[j];
	  res1.a[j] = DEFAULT_VALUE;
	  res2.a[j] = DEFAULT_VALUE;
	  res3.a[j] = DEFAULT_VALUE;

	  CALC (&res_ref[j], res1.a[j], s1.a[j], s2.a[j]);
	}

      res1.x = INTRINSIC (_fixupimm_pd) (res1.x, s1.x, s2.x, 0);
      res2.x = INTRINSIC (_mask_fixupimm_pd) (res2.x, mask, s1.x, s2.x, 0);
      res3.x = INTRINSIC (_maskz_fixupimm_pd) (mask, res3.x, s1.x, s2.x, 0);

      if (UNION_CHECK (AVX512F_LEN, d) (res1, res_ref))
	abort ();

      MASK_MERGE(d) (res_ref, mask, SIZE);
      if (UNION_CHECK (AVX512F_LEN, d) (res2, res_ref))
	abort ();
      MASK_ZERO(d) (res_ref, mask, SIZE);
      if (UNION_CHECK (AVX512F_LEN, d) (res3, res_ref))
	abort ();
    }
}

