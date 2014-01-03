/* Functional tests for the function hotpatching feature.  */

/* { dg-do run } */
/* { dg-options "-O3 -mzarch -mhotpatch=1 --save-temps" } */

#include <stdio.h>

__attribute__ ((hotpatch(2)))
void hp1(void)
{
  printf("hello, world!\n");
}

int main (void)
{
  return 0;
}

/* Check number of occurences of certain instructions.  */
/* { dg-final { scan-assembler-times "nopr\t%r7" 2 } } */
/* { dg-final { scan-assembler-times "nop\t0" 1 } } */
