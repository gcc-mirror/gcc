/* Test the `vceqz_p64' ARM Neon intrinsic.  */

/* { dg-do compile } */
/* { dg-options "-save-temps -O2 -fno-inline" } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"

poly64x1_t v1;
uint64x1_t result1;

void func()
{
  result1 = vceqz_p64 (v1);
}

/* { dg-final { scan-assembler-times "vceq\.i32\[ \t\]+\[dD\]\[0-9\]+, ?\[dD\]\[0-9\]+, #0\n" 1 } } */
