/* { dg-do compile { target { powerpc64le-*-* } } } */
/* { dg-additional-options "-mno-speculate-indirect-jumps" } */

/* Test for deliberate misprediction of indirect calls for ELFv2.  */

extern int (*f)();

int bar ()
{
  return (*f) ();
}

/* { dg-final { scan-assembler "crset eq" } } */
/* { dg-final { scan-assembler "beqctrl-" } } */
