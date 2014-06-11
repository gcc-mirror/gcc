/* Test the crc32d ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-options "-save-temps -O2 -march=armv8-a+crc" } */

#include "arm_acle.h"

uint32_t
test_crc32d (uint32_t arg0, uint64_t arg1)
{
  return __crc32d (arg0, arg1);
}

/* { dg-final { scan-assembler "crc32x\tw..?, w..?, x..?\n" } } */
/* { dg-final { cleanup-saved-temps } } */
