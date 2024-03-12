/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_neon.h>

/* In case where there are no duplicate elements in vector initializer,
   check that the constant is used for duplication.  */

int8x16_t f_s8(int8_t a0, int8_t a1, int8_t a2, int8_t a3, int8_t a4,
                int8_t a5, int8_t a6, int8_t a7, int8_t a8, int8_t a9,
                int8_t a10, int8_t a11, int8_t a12, int8_t a13, int8_t a14)
{
  return (int8x16_t) { a0, a1, a2, a3, a4, a5, a6, a7,
                       a8, a9, a10, a11, a12, a13, a14, 1 };
}

int16x8_t f_s16(int16_t a0, int16_t a1, int16_t a2, int16_t a3, int16_t a4,
		int16_t a5, int16_t a6)
{
  return (int16x8_t) { a0, a1, a2, a3, a4, a5, a6, 1 };
}

int32x4_t f_s32(int32_t a0, int32_t a1, int32_t a2)
{
  return (int32x4_t) { a0, a1, a2, 1 };
}

/* { dg-final { scan-assembler {\tmovi\tv[0-9]+\.8b, 0x1} } } */ 
/* { dg-final { scan-assembler {\tmovi\tv[0-9]+\.4h, 0x1} } } */ 
/* { dg-final { scan-assembler {\tmovi\tv[0-9]+\.2s, 0x1} } } */ 
