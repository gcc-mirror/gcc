/* { dg-do compile } */
/* { dg-skip-if "MUL64 is ARC600 extension." { ! { clmcpu } } } */
/* { dg-options "-O2 -mmul64 -mbig-endian -mcpu=arc600" } */

/* Check if mlo/mhi registers are correctly layout when we compile for
   a big-endian CPU.  */

#include <stdint.h>

uint32_t foo (uint32_t x)
{
  return x % 1000;
}

int32_t bar (int32_t x)
{
  return x % 1000;
}

/* { dg-final { scan-assembler-times "\\s+mul64\\s+" 3 } } */
/* { dg-final { scan-assembler-times "\\s+mulu64\\s+" 1 } } */
/* { dg-final { scan-assembler-times "r\[0-9\]+,mhi" 2 } } */
/* { dg-final { scan-assembler-times "r\[0-9\]+,mlo" 2 } } */
