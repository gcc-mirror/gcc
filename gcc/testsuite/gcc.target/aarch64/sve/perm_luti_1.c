/* { dg-do compile { target { le } } } */
/* { dg-options "-O2 -march=armv9-a+lut -msve-vector-bits=256" } */

typedef __SVUint8_t v32qi __attribute__((arm_sve_vector_bits (256)));
typedef __SVUint16_t v16hi __attribute__((arm_sve_vector_bits (256)));

#define FOUR 2, 0, 3, 1
#define EIGHT 10, 9, 10, 9, 10, 9, 10, 9

v32qi __attribute__((noipa))
luti2_b (v32qi x)
{
  return __builtin_shufflevector (x, x, FOUR, FOUR, FOUR, FOUR,
				 FOUR, FOUR, FOUR, FOUR);
}

v32qi __attribute__((noipa))
luti4_b (v32qi x)
{
  return __builtin_shufflevector (x, x, EIGHT, EIGHT, EIGHT, EIGHT);
}

v16hi __attribute__((noipa))
luti2_h (v16hi x)
{
  return __builtin_shufflevector (x, x, FOUR, FOUR, FOUR, FOUR);
}

v16hi __attribute__((noipa))
luti4_h (v16hi x)
{
  return __builtin_shufflevector (x, x, EIGHT, EIGHT);
}

/* { dg-final { scan-assembler-times {\tluti2\t} 2 } } */
/* { dg-final { scan-assembler-times {\tluti4\t} 2 } } */
/* { dg-final { scan-assembler-not {\ttbl\t} } } */
/* { dg-final { scan-assembler-not {\tld1[rqbhwd]*\t|\tldr\t} } } */
