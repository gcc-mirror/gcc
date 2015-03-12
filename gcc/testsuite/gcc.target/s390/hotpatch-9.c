/* Functional tests for the function hotpatching feature.  */

/* { dg-do compile { target { ! lp64 } } } */
/* { dg-options "-O3 -mesa -march=g5 -mhotpatch=0,4" } */

#include <stdio.h>

void hp1(void)
{
  printf("hello, world!\n");
}

/* Check number of occurences of certain instructions.  */
/* { dg-final { scan-assembler-not "nopr\t%r7" } } */
/* { dg-final { scan-assembler-times "nop\t0" 2 } } */
/* { dg-final { scan-assembler-not "brcl\t0, 0" } } */
