/* Test the crc32cd ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-options "-save-temps -O2 -march=armv8-a+crc" } */

#include "arm_acle.h"

uint32_t
test_crc32cd (uint32_t arg0, uint64_t arg1)
{
  return __crc32cd (arg0, arg1);
}

/* { dg-final { scan-assembler "crc32cx\tw..?, w..?, x..?\n" } } */
/* { dg-final { cleanup-saved-temps } } */
