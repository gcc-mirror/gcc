/* { dg-do compile } */
/* { dg-options "-O2 -mshstk" } */
/* { dg-final { scan-assembler-times "clrssbsy" 1 } } */

#include <immintrin.h>

void f2 (void *__B)
{
  _clrssbsy (__B);
}
