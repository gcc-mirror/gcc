/* Test the `vminnmf32' ARM Neon intrinsic.  */

/* { dg-do run } */
/* { dg-require-effective-target arm_v8_neon_hw } */
/* { dg-options "-save-temps -O3 -march=armv8-a" } */
/* { dg-add-options arm_v8_neon } */

#include "arm_neon.h"

extern void abort ();

void __attribute__ ((noinline))
test_vminnm_f32__regular_input1 ()
{
  float32_t a1[] = {1,2};
  float32_t b1[] = {3,4};
  float32x2_t a = vld1_f32 (a1);
  float32x2_t b = vld1_f32 (b1);
  float32x2_t c = vminnm_f32 (a, b);
  float32_t actual[2];
  vst1_f32 (actual, c);

  for (int i = 0; i < 2; ++i)
    if (actual[i] != a1[i])
      abort ();
}

void __attribute__ ((noinline))
test_vminnm_f32__regular_input2 ()
{
  float32_t a1[] = {3,2};
  float32_t b1[] = {1,4};
  float32_t e[] = {1,2};
  float32x2_t a = vld1_f32 (a1);
  float32x2_t b = vld1_f32 (b1);
  float32x2_t c = vminnm_f32 (a, b);
  float32_t actual[2];
  vst1_f32 (actual, c);

  for (int i = 0; i < 2; ++i)
    if (actual[i] != e[i])
      abort ();
}

void __attribute__ ((noinline))
test_vminnm_f32__quiet_NaN_one_arg ()
{
  /* When given a quiet NaN, vminnm returns the other operand.
     In this test case we have NaNs in only one operand.  */
  float32_t n = __builtin_nanf ("");
  float32_t a1[] = {1,2};
  float32_t b1[] = {n,n};
  float32_t e[] = {1,2};
  float32x2_t a = vld1_f32 (a1);
  float32x2_t b = vld1_f32 (b1);
  float32x2_t c = vminnm_f32 (a, b);
  float32_t actual[2];
  vst1_f32 (actual, c);

  for (int i = 0; i < 2; ++i)
    if (actual[i] != e[i])
      abort ();
}

void __attribute__ ((noinline))
test_vminnm_f32__quiet_NaN_both_args ()
{
  /* When given a quiet NaN, vminnm returns the other operand.
     In this test case we have NaNs in both operands.  */
  float32_t n = __builtin_nanf ("");
  float32_t a1[] = {n,2};
  float32_t b1[] = {1,n};
  float32_t e[] = {1,2};
  float32x2_t a = vld1_f32 (a1);
  float32x2_t b = vld1_f32 (b1);
  float32x2_t c = vminnm_f32 (a, b);
  float32_t actual[2];
  vst1_f32 (actual, c);

  for (int i = 0; i < 2; ++i)
    if (actual[i] != e[i])
      abort ();
}

void __attribute__ ((noinline))
test_vminnm_f32__zero_both_args ()
{
  /* For 0 and -0, vminnm returns -0.  Since 0 == -0, check sign bit.  */
  float32_t a1[] = {0.0,0.0};
  float32_t b1[] = {-0.0, -0.0};
  float32_t e[] = {-0.0, -0.0};

  float32x2_t a = vld1_f32 (a1);
  float32x2_t b = vld1_f32 (b1);
  float32x2_t c = vminnm_f32 (a, b);

  float32_t actual1[2];
  vst1_f32 (actual1, c);

  for (int i = 0; i < 2; ++i)
    if (actual1[i] != e[i] || __builtin_signbit (actual1[i]) == 0)
      abort ();
}

void __attribute__ ((noinline))
test_vminnm_f32__inf_both_args ()
{
  /* The min of inf and inf is inf.  The min of -inf and -inf is -inf.  */
  float32_t inf = __builtin_huge_valf ();
  float32_t a1[] = {inf, -inf};
  float32_t b1[] = {inf, -inf};
  float32_t e[] = {inf, -inf};

  float32x2_t a = vld1_f32 (a1);
  float32x2_t b = vld1_f32 (b1);
  float32x2_t c = vminnm_f32 (a, b);

  float32_t actual1[2];
  vst1_f32 (actual1, c);

  for (int i = 0; i < 2; ++i)
    if (actual1[i] != e[i])
      abort ();
}

void __attribute__ ((noinline))
test_vminnm_f32__two_quiet_NaNs_both_args ()
{
  /* When given 2 NaNs, return a NaN.  Since a NaN is not equal to anything,
     not even another NaN, use __builtin_isnan () to check.  */
  float32_t n = __builtin_nanf ("");
  float32_t a1[] = {n,n};
  float32_t b1[] = {n,n};
  float32_t e[] = {n,n};
  float32x2_t a = vld1_f32 (a1);
  float32x2_t b = vld1_f32 (b1);
  float32x2_t c = vminnm_f32 (a, b);
  float32_t actual[2];
  vst1_f32 (actual, c);

  for (int i = 0; i < 2; ++i)
    if (!__builtin_isnan (actual[i]))
      abort ();
}

int
main ()
{
  test_vminnm_f32__regular_input1 ();
  test_vminnm_f32__regular_input2 ();
  test_vminnm_f32__quiet_NaN_one_arg ();
  test_vminnm_f32__quiet_NaN_both_args ();
  test_vminnm_f32__zero_both_args ();
  test_vminnm_f32__inf_both_args ();
  test_vminnm_f32__two_quiet_NaNs_both_args ();
  return 0;
}

/* { dg-final { scan-assembler-times "vminnm\.f32\t\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+\n" 7 } } */
