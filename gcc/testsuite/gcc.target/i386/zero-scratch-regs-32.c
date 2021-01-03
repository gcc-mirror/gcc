/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -fzero-call-used-regs=all -mno-80387" } */

int
foo (int x)
{
  return (x + 1);
}

/* { dg-final { scan-assembler-not "fldz" } } */

