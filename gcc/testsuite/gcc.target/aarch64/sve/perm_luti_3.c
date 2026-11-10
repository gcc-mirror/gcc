/* { dg-do compile { target le } } */
/* { dg-options "-O2 -march=armv9-a+lut -msve-vector-bits=128" } */

typedef __SVUint16_t v8hi __attribute__((arm_sve_vector_bits (128)));

v8hi __attribute__((noipa))
small_halfword_table (v8hi x)
{
  return __builtin_shufflevector (x, x, 7, 6, 7, 6, 7, 6, 7, 6);
}

v8hi __attribute__((noipa))
two_halfword_tables (v8hi x, v8hi y)
{
  return __builtin_shufflevector (x, y, 2, 9, 2, 9, 2, 9, 2, 9);
}

/* At VL == 128, use the two-register form for both cases.  */
/* { dg-final { scan-assembler-times {\tluti4\tz[0-9]+\.h, \{z[0-9]+\.h - z[0-9]+\.h\}, z[0-9]+\[0\]} 2 } } */
/* { dg-final { scan-assembler-not {\ttbl\t} } } */
