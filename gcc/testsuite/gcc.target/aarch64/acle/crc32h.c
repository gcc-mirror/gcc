/* Test the crc32h ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-options "-save-temps -O2 -march=armv8-a+crc" } */

#include "arm_acle.h"

uint32_t
test_crc32h (uint32_t arg0, uint16_t arg1)
{
  return __crc32h (arg0, arg1);
}

/* { dg-final { scan-assembler "crc32h\tw..?, w..?, w..?\n" } } */
/* { dg-final { cleanup-saved-temps } } */
