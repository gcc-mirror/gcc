/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times "wbinvd" 2 } } */

#include "immintrin.h"

volatile void
test ()
{
  _wbinvd();
}
