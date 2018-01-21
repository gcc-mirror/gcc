/* { dg-do compile { target { ilp32 } } } */
/* { dg-additional-options "-O2 -mno-speculate-indirect-jumps" } */

/* Test for deliberate misprediction of -m32 sibcalls.  */

extern int (*f)();

int bar ()
{
  return (*f) ();
}

/* { dg-final { scan-assembler "crset 2" } } */
/* { dg-final { scan-assembler "beqctr-" } } */
/* { dg-final { scan-assembler {b \$} } } */
