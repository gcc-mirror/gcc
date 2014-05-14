/* { dg-do compile } */
/* { dg-options "-O2 -mclflushopt" } */
/* { dg-final { scan-assembler "clflushopt\[ \\t\]" } } */

#include "x86intrin.h"

void
test_clflushopt (void *__A)
{
  _mm_clflushopt (__A);
}
