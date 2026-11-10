/* { dg-do compile } */
/* { dg-options "-O2 -march=armv9-a+lut -msve-vector-bits=256" } */

typedef __SVUint16_t v16hi __attribute__((arm_sve_vector_bits (256)));

v16hi
first_table_upper_half (v16hi x, v16hi y)
{
  return __builtin_shufflevector (x, y, 10, 17, 10, 17, 10, 17, 10, 17,
				 10, 17, 10, 17, 10, 17, 10, 17);
}

v16hi
second_table_upper_half (v16hi x, v16hi y)
{
  return __builtin_shufflevector (x, y, 2, 25, 2, 25, 2, 25, 2, 25,
				 2, 25, 2, 25, 2, 25, 2, 25);
}

/* The two-register form cannot read beyond the low eight halfwords of
   either table, even though each input vector has sixteen halfwords.  */
/* { dg-final { scan-assembler-not {\tluti[24]\t} } } */
