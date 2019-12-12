/* { dg-do run } */
/* { dg-require-effective-target avx512f } */
/* { dg-options "-O2 -mavx512f" } */

#include "avx512f-check.h"

static inline void __attribute__ ((__always_inline__))
check_cmp (float s1, float s2, const int imm, int expected)
{
  __m128 source1 = _mm_load_ss (&s1);
  __m128 source2 = _mm_load_ss (&s2);
  int res = _mm_comi_round_ss (source1, source2, imm,
			       _MM_FROUND_NO_EXC);
  if (expected != res)
    abort();
}

static void
do_check (float s1, float s2)
{
  check_cmp (s1, s2, _CMP_EQ_OQ,
	     !__builtin_isunordered (s1, s2) && s1 == s2);
  check_cmp (s1, s2, _CMP_LT_OS,
	     !__builtin_isunordered (s1, s2) && s1 < s2);
  check_cmp (s1, s2, _CMP_LE_OS,
	     !__builtin_isunordered (s1, s2) && s1 <= s2);
  check_cmp (s1, s2, _CMP_UNORD_Q,
	     __builtin_isunordered (s1, s2));
  check_cmp (s1, s2, _CMP_NEQ_UQ,
	     __builtin_isunordered (s1, s2) || s1 != s2);
  check_cmp (s1, s2, _CMP_NLT_US,
	     __builtin_isunordered (s1, s2) || s1 >= s2);
  check_cmp (s1, s2, _CMP_NLE_US,
	     __builtin_isunordered (s1, s2) || s1 > s2);
  check_cmp (s1, s2, _CMP_ORD_Q,
	     !__builtin_isunordered (s1, s2));
  check_cmp (s1, s2, _CMP_EQ_UQ,
	     __builtin_isunordered (s1, s2) || s1 == s2);
  check_cmp (s1, s2, _CMP_NGE_US,
	     __builtin_isunordered (s1, s2) || s1 < s2);
  check_cmp (s1, s2, _CMP_NGT_US,
	     __builtin_isunordered (s1, s2) || s1 <= s2);
  check_cmp (s1, s2, _CMP_FALSE_OQ, 0);
  check_cmp (s1, s2, _CMP_NEQ_OQ,
	     !__builtin_isunordered (s1, s2) && s1 != s2);
  check_cmp (s1, s2, _CMP_GE_OS,
	     !__builtin_isunordered (s1, s2) && s1 >= s2);
  check_cmp (s1, s2, _CMP_GT_OS,
	     !__builtin_isunordered (s1, s2) && s1 > s2);
  check_cmp (s1, s2, _CMP_TRUE_UQ, 1);
  check_cmp (s1, s2, _CMP_EQ_OS,
	     !__builtin_isunordered (s1, s2) && s1 == s2);
  check_cmp (s1, s2, _CMP_LT_OQ,
	     !__builtin_isunordered (s1, s2) && s1 < s2);
  check_cmp (s1, s2, _CMP_LE_OQ,
	     !__builtin_isunordered (s1, s2) && s1 <= s2);
  check_cmp (s1, s2, _CMP_UNORD_S,
	     __builtin_isunordered (s1, s2));
  check_cmp (s1, s2, _CMP_NEQ_US,
	     __builtin_isunordered (s1, s2) || s1 != s2);
  check_cmp (s1, s2, _CMP_NLT_UQ,
	     __builtin_isunordered (s1, s2) || s1 >= s2);
  check_cmp (s1, s2, _CMP_NLE_UQ,
	     __builtin_isunordered (s1, s2) || s1 > s2);
  check_cmp (s1, s2, _CMP_ORD_S, !__builtin_isunordered (s1, s2));
  check_cmp (s1, s2, _CMP_EQ_US,
	     __builtin_isunordered (s1, s2) || s1 == s2);
  check_cmp (s1, s2, _CMP_NGE_UQ,
	     __builtin_isunordered (s1, s2) || s1 < s2);
  check_cmp (s1, s2, _CMP_NGT_UQ,
	     __builtin_isunordered (s1, s2) || s1 <= s2);
  check_cmp (s1, s2, _CMP_FALSE_OS, 0);
  check_cmp (s1, s2, _CMP_NEQ_OS,
	     !__builtin_isunordered (s1, s2) && s1 != s2);
  check_cmp (s1, s2, _CMP_GE_OQ,
	     !__builtin_isunordered (s1, s2) && s1 >= s2);
  check_cmp (s1, s2, _CMP_GT_OQ,
	     !__builtin_isunordered (s1, s2) && s1 > s2);
  check_cmp (s1, s2, _CMP_TRUE_US, 1);
}

static void
avx512f_test (void)
{
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
  int i;

  for (i = 0; i < sizeof (inputs) / sizeof (inputs[0]); i++)
    do_check (inputs[i].x1, inputs[i].x2);
}
