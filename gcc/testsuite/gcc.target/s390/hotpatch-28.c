/* Functional tests for the function hotpatching feature.  */

/* { dg-do compile } */
/* { dg-options "-O3 -mzarch -mhotpatch=1,2" } */

void hp1 (volatile unsigned int *i)
{
  for (;;)
    (*i)++;
}

/* Check number of occurences of certain instructions.  */
/* { dg-final { scan-assembler "pre-label NOPs" } } */
/* { dg-final { scan-assembler "^\[^.\].*:\n.*post-label.*(2 halfwords).*\n\(\(.L.*:\n\)\|\(\[\[:space:\]\]*.cfi_.*\n\)\)*\[\[:space:\]\]*nop\t0" } } */
/* { dg-final { scan-assembler-times "nopr\t%r0" 1 } } */
/* { dg-final { scan-assembler-times "nop\t0" 1 } } */
/* { dg-final { scan-assembler-not "brcl\t0, 0" } } */
/* { dg-final { scan-assembler "alignment for hotpatch" } } */
