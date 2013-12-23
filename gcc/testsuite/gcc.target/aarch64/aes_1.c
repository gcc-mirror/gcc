
/* { dg-do compile } */
/* { dg-options "-march=armv8-a+crypto" } */

#include "arm_neon.h"

uint8x16_t
test_vaeseq_u8 (uint8x16_t data, uint8x16_t key)
{
  return vaeseq_u8 (data, key);
}

/* { dg-final { scan-assembler-times "aese\\tv\[0-9\]+\.16b, v\[0-9\]+\.16b" 1 } } */

uint8x16_t
test_vaesdq_u8 (uint8x16_t data, uint8x16_t key)
{
  return vaesdq_u8 (data, key);
}

/* { dg-final { scan-assembler-times "aesd\\tv\[0-9\]+\.16b, v\[0-9\]+\.16b" 1 } } */

uint8x16_t
test_vaesmcq_u8 (uint8x16_t data)
{
  return vaesmcq_u8 (data);
}

/* { dg-final { scan-assembler-times "aesmc\\tv\[0-9\]+\.16b, v\[0-9\]+\.16b" 1 } } */

uint8x16_t
test_vaesimcq_u8 (uint8x16_t data)
{
  return vaesimcq_u8 (data);
}

/* { dg-final { scan-assembler-times "aesimc\\tv\[0-9\]+\.16b, v\[0-9\]+\.16b" 1 } } */


/* { dg-final { cleanup-saved-temps } } */
