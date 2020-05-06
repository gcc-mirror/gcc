/* { dg-do compile } */
/* { dg-options "-O2 -mserialize" } */
/* { dg-final { scan-assembler-times "\tserialize"  1 } } */

#include <x86intrin.h>

void
foo (void)
{
   _serialize ();
}
