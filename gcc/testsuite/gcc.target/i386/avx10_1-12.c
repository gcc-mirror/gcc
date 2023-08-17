/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */

#include <immintrin.h>

__attribute__ ((target ("avx10.1,no-avx512f"))) void
f1 ()
{ /* { dg-warning "'-mno-avx512{f,vl,bw,dq,cd,bf16,fp16,vbmi,vbmi2,vnni,ifma,bitalg,vpopcntdq}' are ignored with '-mavx10.1' and above" } */
  register __m256d a __asm ("ymm17");
  register __m256d b __asm ("ymm16");
  a = _mm256_add_pd (a, b);
  asm volatile ("" : "+v" (a));
}
