/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mxsaves" } */
/* { dg-final { scan-assembler "xsaves64\[ \\t\]" } } */
/* { dg-final { scan-assembler "xrstors64\[ \\t\]" } } */

#include "x86intrin.h"

void
test_xsaves (void *__A, long long __B)
{
  _xsaves64 (__A, __B);
  _xrstors64 (__A, __B);
}
