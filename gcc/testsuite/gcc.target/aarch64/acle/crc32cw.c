/* Test the crc32cw ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-options "-save-temps -O2 -march=armv8-a+crc" } */

#include "arm_acle.h"

uint32_t
test_crc32cw (uint32_t arg0, uint32_t arg1)
{
  return __crc32cw (arg0, arg1);
}

/* { dg-final { scan-assembler "crc32cw\tw..?, w..?, w..?\n" } } */
