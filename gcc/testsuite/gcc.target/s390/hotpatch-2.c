/* Functional tests for the function hotpatching feature.  */

/* { dg-do compile } */
/* { dg-options "-mzarch -mhotpatch=0,1" } */
/* { dg-skip-if "" { *-*-* } { "*" } { "-O*" } } */

#include <stdio.h>

void hp1(void)
{
  printf("hello, world!\n");
}

/* Check number of occurences of certain instructions.  */
/* { dg-final { scan-assembler-not "pre-label NOPs" } } */
/* { dg-final { scan-assembler "^\[^.\].*:\n.*post-label.*(1 halfwords).*\n\(\(.L.*:\n\)\|\(\[\[:space:\]\]*.cfi_.*\n\)\)*\[\[:space:\]\]*nopr\t" } } */
/* { dg-final { scan-assembler-times "nopr\t%r0" 1 } } */
/* { dg-final { scan-assembler-not "nop\t0" } } */
/* { dg-final { scan-assembler-not "brcl\t0, 0" } } */
/* { dg-final { scan-assembler-not "alignment for hotpatch" } } */
