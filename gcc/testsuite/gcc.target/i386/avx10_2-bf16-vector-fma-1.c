/* { dg-do compile } */
/* { dg-options "-march=x86-64-v3 -mavx10.2 -O2" } */
/* { dg-final { scan-assembler-times "vfmadd132bf16\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfmsub132bf16\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmadd132bf16\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmsub132bf16\[ \\t\]+\[^\{\n\]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+\[^\n\r]*%ymm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfmadd132bf16\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfmsub132bf16\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmadd132bf16\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */
/* { dg-final { scan-assembler-times "vfnmsub132bf16\[ \\t\]+\[^\{\n\]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+\[^\n\r]*%xmm\[0-9\]+(?:\n|\[ \\t\]+#)" 1 } } */

#include <immintrin.h>

typedef __bf16 v16bf __attribute__ ((__vector_size__ (32)));
typedef __bf16 v8bf __attribute__ ((__vector_size__ (16)));

v16bf
foo_madd_256 (v16bf a, v16bf b, v16bf c)
{
  return a * b + c;
}

v16bf
foo_msub_256 (v16bf a, v16bf b, v16bf c)
{
  return a * b - c;
}

v16bf
foo_nmadd_256 (v16bf a, v16bf b, v16bf c)
{
  return -a * b + c;
}

v16bf
foo_nmsub_256 (v16bf a, v16bf b, v16bf c)
{
  return -a * b - c;
}

v8bf
foo_madd_128 (v8bf a, v8bf b, v8bf c)
{
  return a * b + c;
}

v8bf
foo_msub_128 (v8bf a, v8bf b, v8bf c)
{
  return a * b - c;
}

v8bf
foo_nmadd_128 (v8bf a, v8bf b, v8bf c)
{
  return -a * b + c;
}

v8bf
foo_nmsub_128 (v8bf a, v8bf b, v8bf c)
{
  return -a * b - c;
}
