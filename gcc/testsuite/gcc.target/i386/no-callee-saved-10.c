/* { dg-do compile { target *-*-linux* } } */
/* { dg-options "-O2 -mgeneral-regs-only -mtune-ctrl=^prologue_using_move,^epilogue_using_move" } */

extern void bar (void) __attribute__ ((no_callee_saved_registers));

__attribute__ ((no_caller_saved_registers))
void
foo (void)
{
  bar ();
}

/* foo must save and restore all caller saved registers since bar won't
   preserve any.  */
/* { dg-final { scan-assembler-not "jmp\[\\t \]+_?bar" } } */
/* { dg-final { scan-assembler "call\[\\t \]+_?bar" } } */
/* { dg-final { scan-assembler-times "push(?:l|q)\[\\t \]*%(?:e|r)ax" 1 } } */
/* { dg-final { scan-assembler-times "push(?:l|q)\[\\t \]*%(?:e|r)bx" 1 } } */
/* { dg-final { scan-assembler-times "push(?:l|q)\[\\t \]*%(?:e|r)cx" 1 } } */
/* { dg-final { scan-assembler-times "push(?:l|q)\[\\t \]*%(?:e|r)dx" 1 } } */
/* { dg-final { scan-assembler-times "push(?:l|q)\[\\t \]*%(?:e|r)bp" 1 } } */
/* { dg-final { scan-assembler-times "push(?:l|q)\[\\t \]*%(?:e|r)si" 1 } } */
/* { dg-final { scan-assembler-times "push(?:l|q)\[\\t \]*%(?:e|r)di" 1 } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r8" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r9" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r10" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r11" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r12" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r13" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r14" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pushq\[\\t \]*%r15" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "pop(?:l|q)\[\\t \]*%(?:e|r)ax" 1 } } */
/* { dg-final { scan-assembler-times "pop(?:l|q)\[\\t \]*%(?:e|r)bx" 1 } } */
/* { dg-final { scan-assembler-times "pop(?:l|q)\[\\t \]*%(?:e|r)cx" 1 } } */
/* { dg-final { scan-assembler-times "pop(?:l|q)\[\\t \]*%(?:e|r)dx" 1 } } */
/* { dg-final { scan-assembler-times "pop(?:l|q)\[\\t \]*%(?:e|r)bp" 1 } } */
/* { dg-final { scan-assembler-times "pop(?:l|q)\[\\t \]*%(?:e|r)si" 1 } } */
/* { dg-final { scan-assembler-times "pop(?:l|q)\[\\t \]*%(?:e|r)di" 1 } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r8" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r9" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r10" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r11" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r12" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r13" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r14" 1 { target { ! ia32 } } } } */
/* { dg-final { scan-assembler-times "popq\[\\t \]*%r15" 1 { target { ! ia32 } } } } */
