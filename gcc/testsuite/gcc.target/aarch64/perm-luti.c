/* { dg-do compile { target { le } } } */
/* { dg-options "-O2 -march=armv9-a+lut" } */

typedef unsigned char v16qi __attribute__((vector_size (16)));
typedef unsigned short v8hi __attribute__((vector_size (16)));

v16qi __attribute__((noipa))
luti2_b (v16qi x)
{
  return __builtin_shufflevector (x, x, 2, 0, 3, 1, 2, 0, 3, 1,
				 2, 0, 3, 1, 2, 0, 3, 1);
}

v16qi __attribute__((noipa))
luti4_b (v16qi x)
{
  return __builtin_shufflevector (x, x, 10, 9, 10, 9, 10, 9, 10, 9,
				 10, 9, 10, 9, 10, 9, 10, 9);
}

v8hi __attribute__((noipa))
luti2_h (v8hi x)
{
  return __builtin_shufflevector (x, x, 2, 0, 3, 1, 2, 0, 3, 1);
}

/* The packed indices can all be materialized with MOVI.  */
/* { dg-final { scan-assembler-times {\tluti2\t} 2 } } */
/* { dg-final { scan-assembler-times {\tluti4\t} 1 } } */
/* { dg-final { scan-assembler-not {\ttbl\t} } } */
/* { dg-final { scan-assembler-not {\tldr\t} } } */
