/* Test that using an Armv8-a hard-float target doesn't
   break CRC intrinsics.  */

/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v8a_crc_hard_ok }  */
/* { dg-add-options arm_arch_v8a_crc_hard }*/

#include <arm_acle.h>

uint32_t
foo (uint32_t a, uint32_t b)
{
  return __crc32cw (a, b);
}
