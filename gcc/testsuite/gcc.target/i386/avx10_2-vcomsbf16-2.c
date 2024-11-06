/* { dg-do run } */
/* { dg-options "-O2 -march=x86-64-v3 -mavx10.2" } */
/* { dg-require-effective-target avx10_2 } */

#define AVX10_2
#define AVX10_SCALAR
#include "avx10-helper.h"
#define SIZE_RES (128 / 16)

#define CMP(PRED, IMM) \
  exp = _mm_comi_round_ss (__A, __B, IMM, _MM_FROUND_NO_EXC); \
  res1 = _mm_com##PRED##_sbh (src1.x, src2.x);		      \
  if (exp != res1)					      \
    abort ();

void
TEST (void)
{
  int i;
  int res1, exp;
  UNION_TYPE (128, bf16_uw) src1, src2;
  
  struct
    {
      float x1;
      float x2;
    }
  inputs[] =
    {
      { 4.3, 2.18 },
      { -4.3, 3.18 },
      { __builtin_nanf (""), -5.8 },
      { -4.8, __builtin_nansf ("") },
      { 3.8, __builtin_nansf ("") },
      { 4.2, 4.2 },
      { __builtin_nanf (""), __builtin_nansf ("") },
    };

  for (i = 0; i < sizeof (inputs) / sizeof (inputs[0]); i++)
    {
      float x = inputs[i].x1;
      float y = inputs[i].x2;

       __m128 __A = _mm_load_ss (&x); 
       __m128 __B = _mm_load_ss (&y); 
      for (int n = 0; n < SIZE_RES; n++)
	{
	  src2.a[n] = convert_fp32_to_bf16(y);
	  src1.a[n] = convert_fp32_to_bf16(x);
	}
      CMP (eq, _CMP_EQ_OQ);
      CMP (ge, _CMP_GE_OS);
      CMP (gt, _CMP_GT_OS);
      CMP (lt, _CMP_LT_OS);
      CMP (le, _CMP_LE_OS);
      CMP (neq, _CMP_NEQ_UQ);
    }
}
