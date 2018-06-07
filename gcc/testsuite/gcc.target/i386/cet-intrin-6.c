/* { dg-do compile } */
/* { dg-options "-O2 -mshstk" } */
/* { dg-final { scan-assembler-times "rstorssp" 1 } } */

#include <immintrin.h>

void f2 (void *__B)
{
  _rstorssp (__B);
}
