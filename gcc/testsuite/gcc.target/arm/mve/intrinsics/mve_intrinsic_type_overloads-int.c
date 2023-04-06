/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2 -Wno-pedantic -Wno-long-long" } */

#include "arm_mve.h"

int i1;
short i2;
long i3;
long long i4;
int8_t i5;
int16_t i6;
int32_t i7;
int64_t i8;

int16x8_t intvec;

void test(void)
{
    /* Test a few different supported ways of passing an int value.  The
    intrinsic vmulq was chosen arbitrarily, but it is representative of
    all intrinsics that take a non-const scalar value.  */
    intvec = vmulq(intvec, 2);
    intvec = vmulq(intvec, (int32_t) 2);
    intvec = vmulq(intvec, (short) 2);
    intvec = vmulq(intvec, i1);
    intvec = vmulq(intvec, i2);
    intvec = vmulq(intvec, i3);
    intvec = vmulq(intvec, i4);
    intvec = vmulq(intvec, i5);
    intvec = vmulq(intvec, i6);
    intvec = vmulq(intvec, i7);
    intvec = vmulq(intvec, i8);
}

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
