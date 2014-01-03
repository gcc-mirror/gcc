/* { dg-do run } */
/* { dg-options "-O2 -mavx512f" } */
/* { dg-require-effective-target avx512f } */

#define AVX512F

#include "avx512f-helper.h"

#define SIZE (AVX512F_LEN / 64)
#include "avx512f-mask-type.h"
#include "math.h"

static void
CALC (double *s, double *r, int imm)
{
  int i = 0, rc, m;
  rc = imm & 0xf;
  m = imm >> 4;
  for (i = 0; i < SIZE; i++)
    switch (rc)
      {
      case _MM_FROUND_FLOOR:
	r[i] = floor (s[i] * pow (2, m)) / pow (2, m);
	break;
      case _MM_FROUND_CEIL:
	r[i] = ceil (s[i] * pow (2, m)) / pow (2, m);
	break;
      default:
	abort ();
	break;
      }
}

void static
TEST (void)
{
  int imm, i, j;
  UNION_TYPE (AVX512F_LEN, d) res1,res2,res3,s;
  double res_ref[SIZE];
  double res_ref_mask[SIZE];

  MASK_TYPE mask = 6 ^ (0xff >> SIZE);

  imm = _MM_FROUND_FLOOR | (7 << 4);

  for (i = 0; i < 3; i++)
    {

      for (j = 0; j < SIZE; j++)
	{
	  s.a[j] = j * (j + 12.0231);
	  res1.a[j] = DEFAULT_VALUE;
	  res2.a[j] = DEFAULT_VALUE;
	  res3.a[j] = DEFAULT_VALUE;
	}

      switch (i)
	{
	case 0:
	  imm = _MM_FROUND_FLOOR | (7 << 4);
	  res1.x = INTRINSIC (_roundscale_pd) (s.x, imm);
	  res2.x = INTRINSIC (_mask_roundscale_pd) (res2.x, mask, s.x, imm);
	  res3.x = INTRINSIC (_maskz_roundscale_pd) (mask, s.x, imm);
	  break;
	case 1:
	  imm = _MM_FROUND_FLOOR;
	  res1.x = INTRINSIC (_floor_pd) (s.x);
	  res2.x = INTRINSIC (_mask_floor_pd) (res2.x, mask, s.x);
	  res3.x = INTRINSIC (_maskz_floor_pd) (mask, s.x);
	  break;
	case 2:
	  imm = _MM_FROUND_CEIL;
	  res1.x = INTRINSIC (_ceil_pd) (s.x);
	  res2.x = INTRINSIC (_mask_ceil_pd) (res2.x, mask, s.x);
	  res3.x = INTRINSIC (_maskz_ceil_pd) (mask, s.x);
	  break;
	}

      CALC (s.a, res_ref, imm);

      if (UNION_CHECK (AVX512F_LEN, d) (res1, res_ref))
	abort ();

      MASK_MERGE(d) (res_ref,mask,SIZE );

      if (UNION_CHECK (AVX512F_LEN, d) (res2, res_ref))
	abort ();

      MASK_ZERO(d) (res_ref,mask,SIZE );

      if (UNION_CHECK (AVX512F_LEN, d) (res3, res_ref))
	abort ();

    }
}

