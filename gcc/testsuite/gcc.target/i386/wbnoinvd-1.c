/* { dg-do compile } */
/* { dg-options "-O2 -mwbnoinvd" } */
/* { dg-final { scan-assembler-times "wbnoinvd" 2 } } */

#include "x86intrin.h"

void test ()
{
  _wbnoinvd();
}
