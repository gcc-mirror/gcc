/* { dg-do compile } */
/* { dg-options "-mno-xbpf" } */

/* GCC should not save and restore callee-saved registers unless
   generating code for xBPF.  */

int
foo ()
{
  register int f asm ("r6");

  f = 20;
  return f + 1;
}

/* { dg-final { scan-assembler-not "stxdw\t\\\[%fp\\+-8\\\],%r6" } } */
/* { dg-final { scan-assembler-not "ldxdw\t%r6,\\\[%fp\\+-8\\\]" } } */
