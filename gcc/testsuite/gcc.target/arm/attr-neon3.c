/* { dg-do compile } */
/* { dg-require-effective-target arm_crypto_ok } */
/* { dg-options "-O2 -mfloat-abi=softfp -mfpu=vfp" } */

#include <arm_neon.h>

/* Check that neon is used.  */
int8x8_t __attribute__ ((target("fpu=neon")))
my (int8x8_t __a, int8x8_t __b)
{
  return __a + __b;
}

/* Check that crypto builtins are recognized.  */
poly128_t __attribute__ ((target("fpu=crypto-neon-fp-armv8")))
foo (poly128_t* ptr)
{
  return vldrq_p128 (ptr);
}

/* Check that default mode is restored.  */
int8x8_t
my1 (int8x8_t __a, int8x8_t __b)
{
  return __a + __b;
}

/* { dg-final { scan-assembler-times "\.fpu vfp" 1 } } */
/* { dg-final { scan-assembler-times "\.fpu neon" 1 } } */
/* { dg-final { scan-assembler-times "\.fpu crypto-neon-fp-armv8" 1 } } */
/* { dg-final { scan-assembler-times "vld1" 1 } } */
/* { dg-final { scan-assembler-times "vadd" 1} } */
