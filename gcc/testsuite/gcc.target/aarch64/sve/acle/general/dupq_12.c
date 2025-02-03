/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target aarch64_little_endian } */

#include <arm_sve.h>

svint16_t
dupq (int x)
{
  return svdupq_s16 (x, 0, x, 0, x, 0, 11, 0);
}

/* { dg-final { scan-assembler {\tmovi\tv[0-9]+\.4s, #?(?:0xb|11)} } } */
