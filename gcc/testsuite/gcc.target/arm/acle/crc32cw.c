/* Test the crc32cw ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-require-effective-target arm_crc_ok } */
/* { dg-options "-save-temps -O0" } */
/* { dg-add-options arm_crc } */

#include "arm_acle.h"

void test_crc32cw (void)
{
  uint32_t out_uint32_t;
  uint32_t arg0_uint32_t;
  uint32_t arg1_uint32_t;

  out_uint32_t = __crc32cw (arg0_uint32_t, arg1_uint32_t);
}

/* { dg-final { scan-assembler "crc32cw\t...?, ...?, ...?\n" } } */
/* { dg-final { cleanup-saved-temps } } */
