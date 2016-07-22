/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -mgeneral-regs-only -mno-cld -mno-iamcu -mpush-args -mno-accumulate-outgoing-args" } */

extern void bar (int) __attribute__ ((no_caller_saved_registers));

void
 __attribute__ ((interrupt))
fn1 (void *frame)
{
  bar (3);
}

void
 __attribute__ ((interrupt))
fn2 (void *frame)
{
  bar (3);
}

/* { dg-final { scan-assembler-not "movups\[\\t .\]*%(x|y|z)mm\[0-9\]+" } } */
/* { dg-final { scan-assembler-not "(push|pop)(l|q)\[\\t \]*%(r|e)(a|b|c|d)x" { target nonpic } } } */
/* { dg-final { scan-assembler-not "(push|pop)l\[\\t \]*%edi" { target ia32 } } } */
/* { dg-final { scan-assembler-not "(push|pop)(l|q)\[\\t \]*%(r|e)si" } } */
/* { dg-final { scan-assembler-not "(push|pop)q\[\\t \]*%r\[8-9\]+" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "(push|pop)q\[\\t \]*%r1\[0-5\]+" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "push(?:l|q)\[\\t \]*%(?:r|e)bp" 2 } } */
/* { dg-final { scan-assembler-times "leave" 2 { target ia32 } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%rdi" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "movq\[\\t \]*-8\\(%(?:r|e)bp\\),\[\\t \]*%rdi" 2 { target { { ! ia32 } && nonpic } } } } */
/* { dg-final { scan-assembler-times "iret" 2 { target ia32 } } } */
/* { dg-final { scan-assembler-times "iretq" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "\tcld" 2 } } */
