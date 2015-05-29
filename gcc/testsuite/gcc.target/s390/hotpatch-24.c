/* Functional tests for the function hotpatching feature.  */

/* { dg-do compile } */
/* { dg-options "-mzarch -mhotpatch=0,1 -falign-functions=2048" } */

#include <stdio.h>

void __attribute__ ((aligned(4096))) hp1(void)
{
  printf("hello, world!\n");
}

/* Check number of occurences of certain instructions.  */
/* { dg-final { scan-assembler-not "alignment for hotpatch" } } */
