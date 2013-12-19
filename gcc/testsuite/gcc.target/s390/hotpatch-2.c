/* Functional tests for the function hotpatching feature.  */

/* { dg-do run } */
/* { dg-options "-O3 -mzarch -mhotpatch=1 --save-temps" } */

#include <stdio.h>

void hp1(void)
{
  printf("hello, world!\n");
}

int main (void)
{
  return 0;
}

/* Check number of occurences of certain instructions.  */
/* { dg-final { scan-assembler-times "nopr\t%r7" 1 } } */
/* { dg-final { scan-assembler-times "nop\t0" 1 } } */
