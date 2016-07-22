/* { dg-do compile } */
/* { dg-options "-O3 -mgeneral-regs-only -mno-cld -mpreferred-stack-boundary=3" { target { ! { ia32 } } } } */
/* { dg-options "-O3 -mgeneral-regs-only -mno-cld -mpreferred-stack-boundary=2" { target { ia32 } } } */

extern void foo (void *) __attribute__ ((interrupt));
extern void bar (void) __attribute__ ((no_caller_saved_registers));

void foo (void *frame)
{
  bar ();
}
/* { dg-final { scan-assembler-not "jmp" } } */
/* { dg-final { scan-assembler-times "iret" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-times "iretq" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "\tcld" 1 } } */
