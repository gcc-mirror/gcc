/* Functional tests for the function hotpatching feature.  */

/* { dg-do compile } */
/* { dg-options "-O3 -mzarch" } */

#include <stdio.h>

__attribute__ ((hotpatch(1,0)))
void hp1(void)
{
  printf("hello, world!\n");
}

/* Check number of occurences of certain instructions.  */
/* { dg-final { scan-assembler-times "nopr\t%r7" 1 } } */
/* { dg-final { scan-assembler-not "nop\t0" } } */
/* { dg-final { scan-assembler-not "brcl\t0, 0" } } */
