/* { dg-do compile } */
/* { dg-options "-O2 -mwbnoinvd" } */
/* { dg-final { scan-assembler-times {\twbnoinvd} 1 } } */

#include "x86intrin.h"

void test ()
{
  _wbnoinvd();
}
