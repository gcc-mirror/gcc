/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -mavx10.2-512 -O2" } */
/* { dg-final { scan-assembler-times "vfmadd132bf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfmsub132bf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmadd132bf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmsub132bf16\[ \\t\]+\[^\{\n\]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+\[^\n\r]*%zmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

typedef __bf16 v32bf __attribute__ ((__vector_size__ (64)));

v32bf
foo_madd (v32bf a, v32bf b, v32bf c)
{
  return a * b + c;
}

v32bf
foo_msub (v32bf a, v32bf b, v32bf c)
{
  return a * b - c;
}

v32bf
foo_nmadd (v32bf a, v32bf b, v32bf c)
{
  return -a * b + c;
}

v32bf
foo_nmsub (v32bf a, v32bf b, v32bf c)
{
  return -a * b - c;
}
