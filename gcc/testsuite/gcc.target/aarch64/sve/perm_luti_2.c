/* { dg-do compile } */
/* { dg-options "-O2 -march=armv9-a+sve2p3 -msve-vector-bits=256" } */

typedef __SVUint8_t v32qi __attribute__((arm_sve_vector_bits (256)));

v32qi __attribute__((noipa))
luti6_b (v32qi x, v32qi y)
{
  return __builtin_shufflevector (x, y,
	0, 35, 2, 39, 4, 43, 6, 47, 8, 51, 10, 55, 12, 59, 14, 63,
	16, 33, 18, 37, 20, 41, 22, 45, 24, 49, 26, 53, 28, 57, 30, 61);
}

/* { dg-final { scan-assembler-times {\tluti6\t} 1 } } */
/* { dg-final { scan-assembler-not {\ttbl\t|\torr\t|\tsub\t} } } */
