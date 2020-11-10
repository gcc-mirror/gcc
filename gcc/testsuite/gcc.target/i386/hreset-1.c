/* { dg-do compile } */
/* { dg-options "-O2 -mhreset" } */
/* { dg-final { scan-assembler-times "eax"  1 } } */
/* { dg-final { scan-assembler-times "hreset\[ \\t\]+\[\$\]\?0"  1 } } */

#include <immintrin.h>

void foo(unsigned int eax)
{
  _hreset (eax);
}
