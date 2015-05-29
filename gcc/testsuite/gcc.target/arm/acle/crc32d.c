/* Test the crc32d ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-require-effective-target arm_crc_ok } */
/* { dg-options "-save-temps -O0" } */
/* { dg-add-options arm_crc } */

#include "arm_acle.h"

void test_crc32d (void)
{
  uint32_t out_uint32_t;
  uint32_t arg0_uint32_t;
  uint64_t arg1_uint64_t;

  out_uint32_t = __crc32d (arg0_uint32_t, arg1_uint64_t);
}

/* { dg-final { scan-assembler-times "crc32w\t...?, ...?, ...?\n" 2 } } */
