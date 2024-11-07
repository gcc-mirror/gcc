/* { dg-do compile } */
/* { dg-options "-msse2 -O2 -ffast-math" } */
/* { dg-final { scan-assembler-times {(?n)(?:vpermi2w|punpcklwd)} 2 { target { ! ia32 } } } } */

typedef float v2sf __attribute__((vector_size(8)));
typedef __bf16 v2bf __attribute__((vector_size(4)));

v2sf
foo (v2bf b, v2bf a)
{
  return __builtin_convertvector (a, v2sf);
}


v2sf
foo_mem (v2bf* a)
{
  return __builtin_convertvector (*a, v2sf);
}

