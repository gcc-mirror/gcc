/* { dg-do compile } */
/* { dg-skip-if "not implemented for Darwin" { powerpc*-*-darwin* } } */
/* { dg-additional-options "-mno-speculate-indirect-jumps" } */
/* { dg-warning "'-mno-speculate-indirect-jumps' is deprecated" "" { target *-*-* } 0 } */

/* Test for deliberate misprediction of indirect calls.  */

extern int (*f)();

int bar ()
{
  return (*f) ();
}

/* { dg-final { scan-assembler "crset 2" } } */

/* The AIX and ELFv2 ABIs don't allow a sibcall here.  */
/* { dg-final { scan-assembler "beqctrl-" { target { lp64 || { powerpc*-*-aix* } } } } } */

/* The other ABIs do allow a sibcall.  */
/* { dg-final { scan-assembler "beqctr-" { target { ilp32 && !powerpc*-*-aix* } } } } */
/* { dg-final { scan-assembler {b \$} { target { ilp32 && !powerpc*-*-aix* } } } } */
