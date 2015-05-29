/* Test the crc32cb ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-require-effective-target arm_crc_ok } */
/* { dg-options "-save-temps -O0" } */
/* { dg-add-options arm_crc } */

#include "arm_acle.h"

void test_crc32cb (void)
{
  uint32_t out_uint32_t;
  uint32_t arg0_uint32_t;
  uint8_t arg1_uint8_t;

  out_uint32_t = __crc32cb (arg0_uint32_t, arg1_uint8_t);
}

/* { dg-final { scan-assembler "crc32cb\t...?, ...?, ...?\n" } } */
