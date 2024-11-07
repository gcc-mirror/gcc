/* { dg-do compile } */
/* { dg-options "-mssse3 -mno-avx512bf16 -mno-avxneconvert -O2 -ffast-math" } */
/* { dg-final { scan-assembler-times {(?n)pshufb} 2 { target { ! ia32 } } } } */

typedef float v2sf __attribute__((vector_size(8)));
typedef __bf16 v2bf __attribute__((vector_size(4)));

v2bf
foo (v2sf b, v2sf a)
{
  return __builtin_convertvector (a, v2bf);
}


v2bf
foo_mem (v2sf* a)
{
  return __builtin_convertvector (*a, v2bf);
}

