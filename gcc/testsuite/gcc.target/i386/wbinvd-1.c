/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-times {\twbinvd} 1 } } */

#include "immintrin.h"

volatile void
test ()
{
  _wbinvd();
}
