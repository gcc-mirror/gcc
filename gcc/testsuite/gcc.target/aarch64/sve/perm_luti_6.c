/* { dg-do compile } */
/* { dg-options "-O2 -march=armv9-a+sve2p3 -msve-vector-bits=512" } */

typedef __SVUint16_t v32hi __attribute__((arm_sve_vector_bits (512)));
typedef __SVFloat16_t v32hf __attribute__((arm_sve_vector_bits (512)));
typedef __SVBfloat16_t v32bf __attribute__((arm_sve_vector_bits (512)));

#define TWO_TABLES(NAME, TYPE) \
TYPE __attribute__((noipa)) \
NAME (TYPE x, TYPE y) \
{ \
  return __builtin_shufflevector (x, y, \
	0, 35, 2, 39, 4, 43, 6, 47, 8, 51, 10, 55, 12, 59, 14, 63, \
	16, 33, 18, 37, 20, 41, 22, 45, 24, 49, 26, 53, 28, 57, 30, 61); \
}

TWO_TABLES (luti6_h, v32hi)
TWO_TABLES (luti6_hf, v32hf)
TWO_TABLES (luti6_bf, v32bf)

/* { dg-final { scan-assembler-times {\tluti6\tz[0-9]+\.h, \{z[0-9]+\.h - z[0-9]+\.h\}, z[0-9]+\[0\]} 3 } } */
/* { dg-final { scan-assembler-not {\ttbl\t|\torr\t|\tsub\t} } } */
