/* Functional tests for the function hotpatching feature.  */

/* { dg-do compile } */
/* { dg-options "-O3 -mzarch --save-temps" } */

#include <stdio.h>

__attribute__ ((hotpatch(0,2)))
void hp1(void)
{
  printf("hello, world!\n");
}

/* Check number of occurences of certain instructions.  */
/* { dg-final { scan-assembler-not "nopr\t%r7" } } */
/* { dg-final { scan-assembler-times "nop\t0" 1 } } */
/* { dg-final { scan-assembler-not "brcl\t\t0,0" } } */
