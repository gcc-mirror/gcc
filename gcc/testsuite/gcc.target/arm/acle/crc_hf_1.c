/* Test that using an Armv8-a hard-float target doesn't
   break CRC intrinsics.  */

/* { dg-do compile } */
/* { dg-require-effective-target arm_hard_vfp_ok }  */
/* { dg-additional-options "-mfloat-abi=hard -march=armv8-a+simd+crc" } */

#include <arm_acle.h>

uint32_t
foo (uint32_t a, uint32_t b)
{
  return __crc32cw (a, b);
}
