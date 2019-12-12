/* { dg-do compile } */
/* { dg-options "-menqcmd -O2" } */
/* { dg-final { scan-assembler-times "\tenqcmd" 1 } } */
/* { dg-final { scan-assembler-times "\tsete" 1 } } */

#include <x86intrin.h>

unsigned int w;
unsigned int array[16];

int
test_enqcmd (void)
{
  return _enqcmd(&w, array);
}

