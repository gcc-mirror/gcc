/* { dg-do compile } */
/* { dg-options "-O2 -mclwb" } */
/* { dg-final { scan-assembler "clwb\[ \\t\]" } } */

#include "x86intrin.h"

void
test_clwb (void *__A)
{
  _mm_clwb (__A);
}
