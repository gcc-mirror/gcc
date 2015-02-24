/* Functional tests for the function hotpatching feature.  */

/* { dg-do compile } */
/* { dg-options "-O3 -mesa -m31 -march=g5 -mhotpatch=0,4 --save-temps" } */

#include <stdio.h>

void hp1(void)
{
  printf("hello, world!\n");
}

/* Check number of occurences of certain instructions.  */
/* { dg-final { scan-assembler-not "nopr\t%r7" } } */
/* { dg-final { scan-assembler-times "nop\t0" 2 } } */
/* { dg-final { scan-assembler-not "brcl\t\t0,0" } } */
