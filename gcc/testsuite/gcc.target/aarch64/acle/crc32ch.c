/* Test the crc32ch ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-options "-save-temps -O2 -march=armv8-a+crc" } */

#include "arm_acle.h"

uint32_t
test_crc32ch (uint32_t arg0, uint16_t arg1)
{
  return __crc32ch (arg0, arg1);
}

/* { dg-final { scan-assembler "crc32ch\tw..?, w..?, w..?\n" } } */
/* { dg-final { cleanup-saved-temps } } */
