/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -mxsavec" } */
/* { dg-final { scan-assembler "xsavec64\[ \\t\]" } } */

#include "x86intrin.h"

void
test_xsavec (void *__A, long long __B)
{
  _xsavec64 (__A, __B);
}
