/* { dg-do compile } */
/* { dg-options "-mavx512f -O2 -ffinite-math-only " } */

typedef float v16sf __attribute__((vector_size(64)));
__attribute__((__vector_size__(16 * sizeof(__bf16)))) __bf16 foo3_mem_a;
v16sf foo3_mem()
{
  return __builtin_convertvector(foo3_mem_a, v16sf);
}
