/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -mgeneral-regs-only -mno-cld -mno-iamcu -mpush-args -maccumulate-outgoing-args" } */

typedef unsigned int uword_t __attribute__ ((mode (__word__)));
extern void bar (int) __attribute__ ((no_caller_saved_registers));

void
 __attribute__ ((interrupt))
fn1 (void *frame, uword_t error)
{
  bar (error);
}

void
 __attribute__ ((interrupt))
fn2 (void *frame, uword_t error)
{
  bar (error);
}

/* { dg-final { scan-assembler-not "movups\[\\t .\]*%(x|y|z)mm\[0-9\]+" } } */
/* { dg-final { scan-assembler-not "(push|pop)(l|q)\[\\t \]*%(r|e)ax" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "(push|pop)(l|q)\[\\t \]*%(r|e)(c|d)x" } } */
/* { dg-final { scan-assembler-not "(push|pop)(l|q)\[\\t \]*%(r|e)si" } } */
/* { dg-final { scan-assembler-not "(push|pop)l\[\\t \]*%edi" { target ia32 } } } */
/* { dg-final { scan-assembler-not "(push|pop)q\[\\t \]*%rax" { target { { ! ia32 } && nonpic } } } } */
/* { dg-final { scan-assembler-not "(push|pop)q\[\\t \]*%r\[0-9\]+" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushl\[\\t \]*%ebp" 2 { target ia32 } } } */
/* { dg-final { scan-assembler-times "leave" 2 { target { ia32 && nonpic } } } } */
/* { dg-final { scan-assembler-times "pushl\[\\t \]*%eax" 2 { target ia32 } } } */
/* { dg-final { scan-assembler-times "movl\[\\t \]*-4\\(%ebp\\),\[\\t \]*%eax" 2 { target { ia32 && nonpic } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%rdi" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%rdi" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler "(addl|leal).*4.*%esp" { target ia32 } } } */
/* { dg-final { scan-assembler "(add|lea)(?:l|q)\[\\t \]*\\\$?8.*,\[\\t \]*%\[re\]?sp" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "iret" 2 { target ia32 } } } */
/* { dg-final { scan-assembler-times "iretq" 2 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "\tcld" 2 } } */
