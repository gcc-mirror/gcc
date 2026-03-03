/* PR target/124335 */
/* { dg-do assemble } */
/* { dg-require-effective-target masm_intel } */
/* { dg-options "-mavx512fp16 -masm=intel" } */
/* { dg-require-effective-target avx512bw } */

#include <x86intrin.h>

__m128h
foo (_Float16 const *x, __mmask8 y)
{
  return _mm_maskz_load_sh (y, x);
}
