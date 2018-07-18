/* { dg-do compile } */
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
