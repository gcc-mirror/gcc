/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512vl -mno-avx512bf16 -mno-avxneconvert -O2 -ffast-math" } */
/* { dg-final { scan-assembler-times {(?n)(?:vpermw|vpshufb)} 6 } } */

typedef float v4sf __attribute__((vector_size(16)));
typedef float v8sf __attribute__((vector_size(32)));
typedef float v16sf __attribute__((vector_size(64)));
typedef __bf16 v4bf __attribute__((vector_size(8)));
typedef __bf16 v8bf __attribute__((vector_size(16)));
typedef __bf16 v16bf __attribute__((vector_size(32)));

v4bf
foo (v4sf b, v4sf a)
{
  return __builtin_convertvector (a, v4bf);
}

v8bf
foo2 (v8sf b, v8sf a)
{
  return __builtin_convertvector (a, v8bf);
}

v16bf
foo3 (v16sf b, v16sf a)
{
  return __builtin_convertvector (a, v16bf);
}

v4bf
foo_mem (v4sf* a)
{
  return __builtin_convertvector (*a, v4bf);
}

v8bf
foo2_mem (v8sf* a)
{
  return __builtin_convertvector (*a, v8bf);
}

v16bf
foo3_mem (v16sf* a)
{
  return __builtin_convertvector (*a, v16bf);
}
