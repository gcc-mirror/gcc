/* { dg-do run } */
/* { dg-options "-mavx512f -O2 -std=gnu99" } */
/* { dg-require-effective-target avx512f } */
/* { dg-require-effective-target c99_runtime } */

#include "avx512f-check.h"
#include "avx512f-helper.h"
#include <math.h>
#include <float.h>
#include "avx512f-mask-type.h"

void
compute_fixupimmps (float *r, float src, int tbl)
{
  switch (tbl & 0xf)
    {
    case 0:
      *r = src;
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
      *r = FLT_MAX;
      break;
    case 15:
      *r = -FLT_MAX;
      break;
    default:
      abort ();
    }
}

void static
avx512f_test (void)
{
  union128 s1, res1, res2, res3;
  union128i_d s2;
  float res_ref[4];
  int i, j, k;

  float vals[2] = { -10, 10 };
  int controls[10] = { 0x11111111,
    0x77777777, 0x88888888, 0x99999999,
    0xaaaaaaaa, 0xbbbbbbbb, 0xcccccccc,
    0xdddddddd, 0xeeeeeeee, 0xffffffff
  };

  MASK_TYPE mask = MASK_VALUE;

  for (i = 0; i < 2; i++)
    {
      s1.a[0] = vals[i];
      res1.a[0] = res2.a[0] = res3.a[0] = DEFAULT_VALUE;
      for (k = 1; k < 4; k++)
	{
	  s1.a[k] = k;
	  s2.a[k] = k;
	  res_ref[k] = k;
	  res1.a[k] = res2.a[k] = res3.a[k] = DEFAULT_VALUE;
	}

      for (j = 0; j < 10; j++)
	{
	  s2.a[0] = controls[j];
	  compute_fixupimmps (&res_ref[0], s1.a[0], s2.a[0]);

	  res1.x = _mm_fixupimm_ss (res1.x, s1.x, s2.x, 0);
	  res2.x = _mm_mask_fixupimm_ss (res2.x, mask, s1.x, s2.x, 0);
	  res3.x = _mm_maskz_fixupimm_ss (mask, res3.x, s1.x, s2.x, 0);

	  if (check_union128 (res1, res_ref))
	    abort ();

	  MASK_MERGE () (res_ref, mask, 1);
	  if (check_union128 (res2, res_ref))
	    abort ();

	  MASK_ZERO () (res_ref, mask, 1);
	  if (check_union128 (res3, res_ref))
	    abort ();
	}
    }
}
