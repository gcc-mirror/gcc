/* { dg-do assemble } */
/* { dg-options "-O2 -mcpu=generic+crypto" } */

#include "arm_acle.h"

/* Make sure that 'crypto' is not required to compile an intrinsic
   from arm_acle.h in a non-crypto function.  Tests that arm_acle.h
   properly clears the architectural features in its initial target
   pragma.  */

__attribute__ ((target ("+crc+nocrypto")))
int
foo (uint32_t a, uint8_t b)
{
  return __crc32b (a, b);
}
