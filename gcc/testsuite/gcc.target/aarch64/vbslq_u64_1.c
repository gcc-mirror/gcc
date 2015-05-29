/* Test if a BSL-like instruction can be generated from a C idiom.  */
/* { dg-do assemble } */
/* { dg-options "--save-temps -O3" } */

#include <arm_neon.h>

/* Folds to BIF.  */

uint32x4_t
vbslq_dummy_u32 (uint32x4_t a, uint32x4_t b, uint32x4_t mask)
{
  return (mask & a) | (~mask & b);
}

/* { dg-final { scan-assembler-times "bif\\tv" 1 } } */

