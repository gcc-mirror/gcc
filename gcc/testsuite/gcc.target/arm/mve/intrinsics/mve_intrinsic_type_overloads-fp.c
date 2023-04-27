/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2 -Wno-pedantic -Wno-long-long -Wno-incompatible-pointer-types" { target c } } */
/* { dg-additional-options "-O2 -Wno-pedantic -Wno-long-long -fpermissive" { target c++ } } */
#include "arm_mve.h"

float f1;
double f2;
float16_t f3;
float32_t f4;
__fp16 f5;
_Float16 f6;

float16x8_t floatvec;

/* Test a few different supported ways of passing a scalar int value.
The intrinsic vmulq was chosen arbitrarily, but it is representative of
all intrinsics that take a non-const scalar value.  */
void
test_scalars (void)
{
    /* Test a few different supported ways of passing a float value.  */
    floatvec = vmulq(floatvec, 0.5);
    floatvec = vmulq(floatvec, 0.5f);
    floatvec = vmulq(floatvec, (__fp16) 0.5);
    floatvec = vmulq(floatvec, f1);
    floatvec = vmulq(floatvec, f2);
    floatvec = vmulq(floatvec, f3);
    floatvec = vmulq(floatvec, f4);
    floatvec = vmulq(floatvec, f5);
    floatvec = vmulq(floatvec, f6);
    floatvec = vmulq(floatvec, 0.15f16);
    floatvec = vmulq(floatvec, (_Float16) 0.15);
}

/* Next, test a number of valid pointer overloads.  */
void
foo11 (__fp16 * addr, float16x8_t value)
{
  vst1q (addr, value);
}

#ifndef __cplusplus
void
foo12 (_Float16 * addr, float16x8_t value)
{
  vst1q (addr, value);
}
#endif

void
foo13 (float * addr, float32x4_t value)
{
  vst1q (addr, value);
}

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
