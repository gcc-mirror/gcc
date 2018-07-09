/* { dg-do compile } */
/* { dg-options "-O2 -mcldemote" } */
/* { dg-final { scan-assembler "cldemote\[ \\t\]" } } */

#include "x86intrin.h"

void
test_cldemote (void *__A)
{
  _cldemote (__A);
}
