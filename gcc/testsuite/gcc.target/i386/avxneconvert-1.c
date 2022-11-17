/* { dg-do compile } */
/* { dg-options "-mavxneconvert -O2" } */

typedef float v8sf __attribute__((vector_size(32)));
typedef __bf16 v8bf __attribute__((vector_size(16)));

v8bf
foo (v8sf a)
{
  return __builtin_ia32_cvtneps2bf16_v8sf (a);
}
