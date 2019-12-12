/* { dg-do compile } */
/* { dg-options "-menqcmd -O2" } */
/* { dg-final { scan-assembler-times "\tenqcmds" 1 } } */
/* { dg-final { scan-assembler-times "\tsete" 1 } } */

#include <x86intrin.h>

unsigned int w;
unsigned int array[16];
int
test_enqcmds (void)
{
  return _enqcmds(&w, array);
}

