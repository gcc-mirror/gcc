/* Test the crc32b ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-require-effective-target arm_crc_ok } */
/* { dg-options "-save-temps -O0" } */
/* { dg-add-options arm_crc } */

#include "arm_acle.h"

void test_crc32b (void)
{
  uint32_t out_uint32_t;
  uint32_t arg0_uint32_t;
  uint8_t arg1_uint8_t;

  out_uint32_t = __crc32b (arg0_uint32_t, arg1_uint8_t);
}

/* { dg-final { scan-assembler "crc32b\t...?, ...?, ...?\n" } } */
/* { dg-final { cleanup-saved-temps } } */
