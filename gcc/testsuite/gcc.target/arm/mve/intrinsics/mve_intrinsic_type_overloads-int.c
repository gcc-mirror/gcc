/* { dg-require-effective-target arm_v8_1m_mve_ok } */
/* { dg-add-options arm_v8_1m_mve } */
/* { dg-additional-options "-O2 -Wno-pedantic -Wno-long-long -Wno-incompatible-pointer-types" { target c } } */
/* { dg-additional-options "-O2 -Wno-pedantic -Wno-long-long -fpermissive" { target c++ } } */
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

/* Test a few different supported ways of passing a scalar int value.
The intrinsic vmulq was chosen arbitrarily, but it is representative of
all intrinsics that take a non-const scalar value.  */
void
test_scalars (void)
{
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

/* Next, test a number of valid pointer overloads.  */
void
foo1 (signed char * addr, int8x16_t value)
{
  vst1q (addr, value);
}

void
foo2 (short * addr, int16x8_t value)
{
  vst1q (addr, value);
}

/* Glibc defines int32_t as 'int' while newlib defines it as 'long int'.

   Although these correspond to the same size, g++ complains when using the
   'wrong' version:
  invalid conversion from 'long int*' to 'int32_t*' {aka 'int*'} [-fpermissive]

  The trick below is to make this test pass whether using glibc-based or
  newlib-based toolchains.  */

#if defined(__GLIBC__)
#define word_type int
#else
#define word_type long int
#endif
void
foo3 (word_type * addr, int32x4_t value)
{
  vst1q (addr, value);
}

void
foo5 (long long * addr, uint64x2_t value)
{
  vldrdq_gather_offset (addr, value);
}

void
foo6 (unsigned char * addr, uint8x16_t value)
{
  vst1q (addr, value);
}

void
foo7 (unsigned short * addr, uint16x8_t value)
{
  vst1q (addr, value);
}

void
foo8 (unsigned word_type * addr, uint32x4_t value)
{
  vst1q (addr, value);
}

void
foo10 (unsigned long long * addr, uint64x2_t value)
{
  vldrdq_gather_offset (addr, value);
}

/* { dg-final { scan-assembler-not "__ARM_undef" } } */
