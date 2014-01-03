/* Test the crc32ch ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-require-effective-target arm_crc_ok } */
/* { dg-options "-save-temps -O0" } */
/* { dg-add-options arm_crc } */

#include "arm_acle.h"

void test_crc32ch (void)
{
  uint32_t out_uint32_t;
  uint32_t arg0_uint32_t;
  uint16_t arg1_uint16_t;

  out_uint32_t = __crc32ch (arg0_uint32_t, arg1_uint16_t);
}

/* { dg-final { scan-assembler "crc32ch\t...?, ...?, ...?\n" } } */
/* { dg-final { cleanup-saved-temps } } */
