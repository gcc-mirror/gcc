/* { dg-do compile } */
/* { dg-options "-O2 -mxsavec" } */
/* { dg-final { scan-assembler "xsavec\[ \\t\]" } } */

#include "x86intrin.h"

void
test_xsavec (void *__A, long long __B)
{
  _xsavec (__A, __B);
}
