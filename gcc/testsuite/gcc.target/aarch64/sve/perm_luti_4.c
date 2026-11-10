/* { dg-do compile { target le } } */
/* { dg-options "-O2 -march=armv9-a+lut -msve-vector-bits=256" } */

typedef __SVUint16_t v16hi __attribute__((arm_sve_vector_bits (256)));
typedef __SVFloat16_t v16hf __attribute__((arm_sve_vector_bits (256)));
typedef __SVBfloat16_t v16bf __attribute__((arm_sve_vector_bits (256)));

#define TWO_TABLES(NAME, TYPE) \
TYPE __attribute__((noipa)) \
NAME (TYPE x, TYPE y) \
{ \
  return __builtin_shufflevector (x, y, 2, 17, 2, 17, 2, 17, 2, 17, \
				 2, 17, 2, 17, 2, 17, 2, 17); \
}

TWO_TABLES (luti4_h2, v16hi)
TWO_TABLES (luti4_hf2, v16hf)
TWO_TABLES (luti4_bf2, v16bf)

/* Indices into Y start at 16, but LUTI4 needs them to start at 8.  */
/* { dg-final { scan-assembler-times {\tluti4\tz[0-9]+\.h, \{z[0-9]+\.h - z[0-9]+\.h\}, z[0-9]+\[0\]} 3 } } */
/* { dg-final { scan-assembler-not {\ttbl\t|\tld1[rqbhwd]*\t|\tldr\t} } } */
