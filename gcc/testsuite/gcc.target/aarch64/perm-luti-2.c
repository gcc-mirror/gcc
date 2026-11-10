/* { dg-do compile { target le } } */
/* { dg-options "-O2 -march=armv9-a+lut" } */

typedef unsigned short v8hi __attribute__((vector_size (16)));
typedef __fp16 v8hf __attribute__((vector_size (16)));
typedef __bf16 v8bf __attribute__((vector_size (16)));

v8hi __attribute__((noipa))
luti4_h (v8hi x)
{
  return __builtin_shufflevector (x, x, 6, 5, 6, 5, 6, 5, 6, 5);
}

#define TWO_TABLES(NAME, TYPE) \
TYPE __attribute__((noipa)) \
NAME (TYPE x, TYPE y) \
{ \
  return __builtin_shufflevector (x, y, 2, 9, 2, 9, 2, 9, 2, 9); \
}

TWO_TABLES (luti4_h2, v8hi)
TWO_TABLES (luti4_hf2, v8hf)
TWO_TABLES (luti4_bf2, v8bf)

/* { dg-final { scan-assembler-times {\tluti4\tv[0-9]+\.8h, \{v[0-9]+\.8h, v[0-9]+\.8h\}, v[0-9]+\[0\]} 4 } } */
/* { dg-final { scan-assembler-not {\ttbl\t|\tldr\t} } } */
