/* { dg-do compile { target le } } */
/* { dg-options "-O2 -march=armv9-a+lut -msve-vector-bits=scalable" } */

#include <arm_sve.h>

/* Repeat the indices across the full scalable result.  */
svuint8_t __attribute__((noipa))
luti2_b (svuint8_t x)
{
  return __builtin_shuffle (x, svdupq_n_u8 (2, 0, 3, 1, 2, 0, 3, 1,
					  2, 0, 3, 1, 2, 0, 3, 1));
}

svuint8_t __attribute__((noipa))
luti4_b (svuint8_t x)
{
  return __builtin_shuffle (x, svdupq_n_u8 (10, 9, 10, 9, 10, 9, 10, 9,
					  10, 9, 10, 9, 10, 9, 10, 9));
}

svuint16_t __attribute__((noipa))
luti2_h (svuint16_t x)
{
  return __builtin_shuffle (x, svdupq_n_u16 (2, 0, 3, 1, 2, 0, 3, 1));
}

/* VL can be 128, so use two copies of X for the halfword LUTI4 table.  */
svuint16_t __attribute__((noipa))
luti4_h (svuint16_t x)
{
  return __builtin_shuffle (x, svdupq_n_u16 (7, 6, 7, 6, 7, 6, 7, 6));
}

/* The permutation index of Y[1] depends on VL, but its LUTI4 index is 9.  */
svuint16_t __attribute__((noipa))
luti4_h2 (svuint16_t x, svuint16_t y)
{
  unsigned short n = svcnth () + 1;
  return __builtin_shuffle (x, y, svdupq_n_u16 (2, n, 2, n, 2, n, 2, n));
}

/* { dg-final { scan-assembler-times {\tluti2\tz[0-9]+\.b,} 1 } } */
/* { dg-final { scan-assembler-times {\tluti2\tz[0-9]+\.h,} 1 } } */
/* { dg-final { scan-assembler-times {\tluti4\tz[0-9]+\.b,} 1 } } */
/* { dg-final { scan-assembler-times {\tluti4\tz[0-9]+\.h, \{z[0-9]+\.h - z[0-9]+\.h\}, z[0-9]+\[0\]} 2 } } */
/* { dg-final { scan-assembler-not {\ttbl\t|\tld1[rqbhwd]*\t|\tldr\t} } } */
