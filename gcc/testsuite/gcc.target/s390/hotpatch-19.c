/* Functional tests for the function hotpatching feature.  */

/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -mhotpatch=1,2" } */

#include <stdio.h>

/* { dg-prune-output "always_inline function might not be inlinable" } */
__attribute__ ((always_inline))
static void hp2(void)
{
  printf("hello, world!\n");
}

void hp1(void)
{
  hp2();
}

/* Check number of occurences of certain instructions.  */
/* { dg-final { scan-assembler "pre-label.*(1 halfwords)" } } */
/* { dg-final { scan-assembler "post-label.*(2 halfwords)" } } */
/* { dg-final { scan-assembler-times "nopr\t%r7" 1 } } */
/* { dg-final { scan-assembler-times "nop\t0" 1 } } */
/* { dg-final { scan-assembler-not "brcl\t0, 0" } } */
