/* { dg-do compile } */
/* { dg-options "-O2 -mpcommit" } */
/* { dg-final { scan-assembler "pcommit" } } */

#include "x86intrin.h"

void
test_pcommit ()
{
  _mm_pcommit ();
}
