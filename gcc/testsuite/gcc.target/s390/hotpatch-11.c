/* Functional tests for the function hotpatching feature.  */

/* { dg-do run } */
/* { dg-options "-O3 -mzarch -mhotpatch -mno-hotpatch --save-temps" } */

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
/* { dg-final { scan-assembler-not "nopr\t%r7" } } */
/* { dg-final { scan-assembler-not "nop\t0" } } */
