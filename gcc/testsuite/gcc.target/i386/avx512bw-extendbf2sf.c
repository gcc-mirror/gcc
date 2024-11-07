/* { dg-do compile } */
/* { dg-options "-mavx512bw -mavx512vl -O2 -ffast-math" } */
/* { dg-final { scan-assembler-times {(?n)(?:vpermi2w|vpunpcklwd)} 6 } } */

typedef float v4sf __attribute__((vector_size(16)));
typedef float v8sf __attribute__((vector_size(32)));
typedef float v16sf __attribute__((vector_size(64)));
typedef __bf16 v4bf __attribute__((vector_size(8)));
typedef __bf16 v8bf __attribute__((vector_size(16)));
typedef __bf16 v16bf __attribute__((vector_size(32)));

v4sf
foo (v4bf b, v4bf a)
{
  return __builtin_convertvector (a, v4sf);
}

v8sf
foo2 (v8bf b, v8bf a)
{
  return __builtin_convertvector (a, v8sf);
}

v16sf
foo3 (v16bf b, v16bf a)
{
  return __builtin_convertvector (a, v16sf);
}

v4sf
foo_mem (v4bf* a)
{
  return __builtin_convertvector (*a, v4sf);
}

v8sf
foo2_mem (v8bf* a)
{
  return __builtin_convertvector (*a, v8sf);
}

v16sf
foo3_mem (v16bf* a)
{
  return __builtin_convertvector (*a, v16sf);
}
