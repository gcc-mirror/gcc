/* Test the crc32w ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-options "-save-temps -O2 -march=armv8-a+crc" } */

#include "arm_acle.h"

uint32_t
test_crc32w (uint32_t arg0, uint32_t arg1)
{
  return __crc32w (arg0, arg1);
}

/* { dg-final { scan-assembler "crc32w\tw..?, w..?, w..?\n" } } */
/* { dg-final { cleanup-saved-temps } } */
