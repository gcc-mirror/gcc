/* { dg-options "-O2 -msve-vector-bits=256" } */

#include <arm_sve.h>
typedef svbfloat16_t vls_bfloat16_t __attribute__((arm_sve_vector_bits(32 * 8)));
svbfloat16_t foo(vls_bfloat16_t a, vls_bfloat16_t b)
{
  svbfloat16_t zero = svreinterpret_bf16_f32 (svdup_n_f32 (0.0f));
  return svzip2_bf16(zero, svuzp1_bf16(a,b));
}


/* { dg-final { scan-assembler-times {\tuzp1\t} 1 } } */
/* { dg-final { scan-assembler-times {\tzip2\t} 1 } } */
/* { dg-final { scan-assembler-not {\ttbl\t} } } */
