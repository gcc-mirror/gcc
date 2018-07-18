/* { dg-do compile } */
/* { dg-options "-O2 -mshstk" } */
/* { dg-final { scan-assembler-times "wrssd" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "wrss\[d|q]" 2 { target lp64 } } } */

#include <immintrin.h>

void f1 (unsigned int __A, void *__B)
{
  _wrssd (__A, __B);
}

#ifdef __x86_64__
void f2 (unsigned long long __A, void *__B)
{
  _wrssq (__A, __B);
}
#endif
