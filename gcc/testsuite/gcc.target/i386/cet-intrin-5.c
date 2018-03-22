/* { dg-do compile } */
/* { dg-options "-O2 -mcet" } */
/* { dg-final { scan-assembler-times "saveprevssp" 1 } } */

#include <immintrin.h>

void f2 (void)
{
  _saveprevssp ();
}
