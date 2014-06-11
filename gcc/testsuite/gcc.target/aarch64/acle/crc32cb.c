/* Test the crc32cb ACLE intrinsic.  */

/* { dg-do assemble } */
/* { dg-options "-save-temps -O2 -march=armv8-a+crc" } */

#include "arm_acle.h"

uint32_t
test_crc32cb (uint32_t arg0, uint8_t arg1)
{
  return __crc32cb (arg0, arg1);
}

/* { dg-final { scan-assembler "crc32cb\tw..?, w..?, w..?\n" } } */
/* { dg-final { cleanup-saved-temps } } */
