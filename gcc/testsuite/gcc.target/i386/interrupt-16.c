/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -mgeneral-regs-only -mno-cld -mno-iamcu -mno-push-args -maccumulate-outgoing-args" } */

extern int foo (int) __attribute__ ((no_caller_saved_registers));
extern int bar (int) __attribute__ ((no_caller_saved_registers));

int
foo (int i)
{
  return bar (i);
}

/* { dg-final { scan-assembler-not "movups\[\\t \]*%(x|y|z)mm\[0-9\]+,\[\\t \]-*\[0-9\]*\\(%\[re\]?bp\\)" } } */
/* { dg-final { scan-assembler-not "movups\[\\t \]*-\[0-9\]*\\(%\[re\]?bp\\),\[\\t \]*%(x|y|z)mm\[0-9\]+" } } */
/* { dg-final { scan-assembler-not "(push|pop)(l|q)\[\\t \]*%(r|e)bx" { target { nonpic || { ! ia32 } } } } } */
/* { dg-final { scan-assembler-not "(push|pop)(l|q)\[\\t \]*%(r|e)(a|c|d)x" } } */
/* { dg-final { scan-assembler-not "(push|pop)(l|q)\[\\t \]*%(r|e)si" } } */
/* { dg-final { scan-assembler-not "(push|pop)(l|q)\[\\t \]*%(r|e)bp" } } */
/* { dg-final { scan-assembler-not "(push|pop)l\[\\t \]*%edi" { target ia32 } } } */
/* { dg-final { scan-assembler-not "(push|pop)q\[\\t \]*%r\[0-9\]+" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%rdi" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%rdi" 1 { target { ! ia32 } } } } */
