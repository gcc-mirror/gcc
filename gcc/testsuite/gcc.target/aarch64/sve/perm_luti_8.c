/* { dg-do compile { target le } } */
/* { dg-options "-O2 -march=armv9-a+lut -msve-vector-bits=scalable" } */

#include <arm_sve.h>

/* At VL == 128 these indices wrap to X[2] and X[1].  At larger VLs they
   select X[10] and X[9], which a duplicated two-register table cannot read.
   The single-register halfword LUTI4 requires VL >= 256.  */
svuint16_t __attribute__((noipa))
upper_halfwords (svuint16_t x)
{
  return __builtin_shuffle (x, svdupq_n_u16 (10, 9, 10, 9, 10, 9, 10, 9));
}

/* Byte LUTI4 cannot read beyond the low sixteen bytes of its table.  */
svuint8_t __attribute__((noipa))
upper_bytes (svuint8_t x)
{
  return __builtin_shuffle (x, svdupq_n_u8 (18, 17, 18, 17, 18, 17, 18, 17,
					  18, 17, 18, 17, 18, 17, 18, 17));
}

/* { dg-final { scan-assembler-not {\tluti[246]\t} } } */
/* { dg-final { scan-assembler-times {\ttbl\t} 2 } } */
