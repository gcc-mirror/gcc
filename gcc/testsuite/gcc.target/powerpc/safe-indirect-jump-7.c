/* { dg-do compile } */
/* { dg-skip-if "not implemented for Darwin" { powerpc*-*-darwin* } } */
/* { dg-additional-options "-mno-speculate-indirect-jumps" } */
/* { dg-warning "'-mno-speculate-indirect-jumps' is deprecated" "" { target *-*-* } 0 } */

/* Test for deliberate misprediction of indirect calls.  */

extern int (*f)();

int bar ()
{
  return (*f) () * 53;
}

/* { dg-final { scan-assembler "crset 2" } } */
/* { dg-final { scan-assembler "beqctrl-" } } */
