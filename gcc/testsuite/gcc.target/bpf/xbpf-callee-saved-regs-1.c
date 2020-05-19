/* { dg-do compile } */
/* { dg-options "-mxbpf" } */

/* GCC should save and restore callee-saved registers when generating
   code for xBPF.  */

int
foo ()
{
  register int f asm ("r6");

  f = 20;
  return f + 1;
}

/* { dg-final { scan-assembler "stxdw\t\\\[%fp\\+-8\\\],%r6" } } */
/* { dg-final { scan-assembler "ldxdw\t%r6,\\\[%fp\\+-8\\\]" } } */
