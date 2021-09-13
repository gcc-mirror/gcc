/* { dg-do run { target avx512fp16 } } */
/* { dg-options "-O2 -mavx512fp16 -mavx512dq" } */


#define AVX512FP16
#include "avx512fp16-helper.h"


#define CMP(imm, rel)					\
  dst_ref = 0;						\
  dst_ref = ((int) rel) | dst_ref;			\
  dst = _mm_comi_round_sh(src1.xmmh[0], src2.xmmh[0], imm,    \
			  _MM_FROUND_NO_EXC);		\
  if (dst_ref != dst) abort();				\

void
test_512 (void)
{
  V512 v1,v2,v3,v4;
  float s1,s2;
  int res,exp,dst;
  __mmask8 dst_ref;

  init_src_nanf();
  unpack_ph_2twops(src1, &v1, &v2);
  unpack_ph_2twops(src2, &v3, &v4);
  s1 = v1.f32[0];
  s2 = v3.f32[0];

  CMP(_CMP_EQ_OQ, !isunordered(s1, s2) && s1 == s2);
  CMP(_CMP_LT_OS, !isunordered(s1, s2) && s1 < s2);
  CMP(_CMP_LE_OS, !isunordered(s1, s2) && s1 <= s2);
  CMP(_CMP_UNORD_Q, isunordered(s1, s2));
  CMP(_CMP_NEQ_UQ, isunordered(s1, s2) || s1 != s2);
  CMP(_CMP_NLT_US, isunordered(s1, s2) || s1 >= s2);
  CMP(_CMP_NLE_US, isunordered(s1, s2) || s1 > s2);
  CMP(_CMP_ORD_Q, !isunordered(s1, s2));

  CMP(_CMP_EQ_UQ, isunordered(s1, s2) || s1 == s2);
  CMP(_CMP_NGE_US, isunordered(s1, s2) || s1 < s2);
  CMP(_CMP_NGT_US, isunordered(s1, s2) || s1 <= s2);

  CMP(_CMP_FALSE_OQ, 0);
  CMP(_CMP_NEQ_OQ, !isunordered(s1, s2) && s1 != s2);
  CMP(_CMP_GE_OS, !isunordered(s1, s2) && s1 >= s2);
  CMP(_CMP_GT_OS, !isunordered(s1, s2) && s1 > s2);
  CMP(_CMP_TRUE_UQ, 1);

  CMP(_CMP_EQ_OS, !isunordered(s1, s2) && s1 == s2);
  CMP(_CMP_LT_OQ, !isunordered(s1, s2) && s1 < s2);
  CMP(_CMP_LE_OQ, !isunordered(s1, s2) && s1 <= s2);
  CMP(_CMP_UNORD_S, isunordered(s1, s2));
  CMP(_CMP_NEQ_US, isunordered(s1, s2) || s1 != s2);
  CMP(_CMP_NLT_UQ, isunordered(s1, s2) || s1 >= s2);
  CMP(_CMP_NLE_UQ, isunordered(s1, s2) || s1 > s2);
  CMP(_CMP_ORD_S, !isunordered(s1, s2));
  CMP(_CMP_EQ_US, isunordered(s1, s2) || s1 == s2);
  CMP(_CMP_NGE_UQ, isunordered(s1, s2) || s1 < s2);
  CMP(_CMP_NGT_UQ, isunordered(s1, s2) || s1 <= s2);
  CMP(_CMP_FALSE_OS, 0);
  CMP(_CMP_NEQ_OS, !isunordered(s1, s2) && s1 != s2);
  CMP(_CMP_GE_OQ, !isunordered(s1, s2) && s1 >= s2);
  CMP(_CMP_GT_OQ, !isunordered(s1, s2) && s1 > s2);
  CMP(_CMP_TRUE_US, 1);
}

