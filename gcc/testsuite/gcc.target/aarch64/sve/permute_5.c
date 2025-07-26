/* { dg-options "-O -msve-vector-bits=256" } */

typedef __SVBfloat16_t vbfloat16 __attribute__((arm_sve_vector_bits(256)));

vbfloat16
foo (vbfloat16 x, vbfloat16 y)
{
  return __builtin_shufflevector (x, y, 0, 2, 1, 3, 16, 19, 17, 18,
				  8, 9, 10, 11, 23, 22, 21, 20);
}
