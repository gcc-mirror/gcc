/* { dg-do compile } */
/* { dg-options "-O2 -mxsaves" } */
/* { dg-final { scan-assembler "xsaves\[ \\t\]" } } */
/* { dg-final { scan-assembler "xrstors\[ \\t\]" } } */

#include "x86intrin.h"

void
test_xsaves (void *__A, long long __B)
{
  _xsaves (__A, __B);
  _xrstors (__A, __B);
}
