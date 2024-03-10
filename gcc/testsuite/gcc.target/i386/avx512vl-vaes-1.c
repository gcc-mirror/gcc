/* { dg-do compile } */
/* { dg-options "-mvaes -mno-avx512vl -mno-aes" } */

#include <immintrin.h>

typedef long long v2di __attribute__((vector_size (16)));

v2di
f1 (v2di x, v2di y)
{
  return __builtin_ia32_aesenc128 (x, y); /* { dg-error "needs isa option" } */
}
