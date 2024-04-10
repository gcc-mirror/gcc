/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -mtune-ctrl=^prologue_using_move,^epilogue_using_move" } */

extern void bar (void) __attribute__ ((no_callee_saved_registers));

void
foo (void)
{
  bar ();
}

/* foo must save and restore all caller saved registers since bar won't
   preserve any.  */
/* { dg-final { scan-assembler-not "jmp\[\\t \]+_?bar" } } */
/* { dg-final { scan-assembler "call\[\\t \]+_?bar" } } */
/* { dg-final { scan-assembler-not "push(?:l|q)\[\\t \]*%(?:e|r)ax" } } */
/* { dg-final { scan-assembler-times "push(?:l|q)\[\\t \]*%(?:e|r)bx" 1 } } */
/* { dg-final { scan-assembler-not "push(?:l|q)\[\\t \]*%(?:e|r)cx" } } */
/* { dg-final { scan-assembler-not "push(?:l|q)\[\\t \]*%(?:e|r)dx" } } */
/* { dg-final { scan-assembler-times "push(?:l|q)\[\\t \]*%(?:e|r)bp" 1 } } */
/* { dg-final { scan-assembler-times "pushl\[\\t \]*%esi" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-not "pushq\[\\t \]*%rsi" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushl\[\\t \]*%edi" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-not "pushq\[\\t \]*%rdi" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "pushq\[\\t \]*%r8" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "pushq\[\\t \]*%r9" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "pushq\[\\t \]*%r10" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "pushq\[\\t \]*%r11" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r12" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r13" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r14" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r15" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "pop(?:l|q)\[\\t \]*%(?:e|r)ax" } } */
/* { dg-final { scan-assembler-times "pop(?:l|q)\[\\t \]*%(?:e|r)bx" 1 } } */
/* { dg-final { scan-assembler-not "pop(?:l|q)\[\\t \]*%(?:e|r)cx" } } */
/* { dg-final { scan-assembler-not "pop(?:l|q)\[\\t \]*%(?:e|r)dx" } } */
/* { dg-final { scan-assembler-times "pop(?:l|q)\[\\t \]*%(?:e|r)bp" 1 } } */
/* { dg-final { scan-assembler-times "popl\[\\t \]*%esi" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-not "popq\[\\t \]*%rsi" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popl\[\\t \]*%edi" 1 { target ia32 } } } */
/* { dg-final { scan-assembler-not "popq\[\\t \]*%rdi" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "popq\[\\t \]*%r8" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "popq\[\\t \]*%r9" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "popq\[\\t \]*%r10" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-not "popq\[\\t \]*%r11" { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r12" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r13" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r14" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r15" 1 { target { ! ia32 } } } } */
