/* { dg-do compile } */
/* { dg-options "-O2 -mcet" } */
/* { dg-final { scan-assembler-times "wrussd" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "wruss\[d|q]" 2 { target lp64 } } } */

#include <immintrin.h>

void f1 (unsigned int __A, void *__B)
{
  _wrussd (__A, __B);
}

#ifdef __x86_64__
void f2 (unsigned long long __A, void *__B)
{
  _wrussq (__A, __B);
}
#endif
