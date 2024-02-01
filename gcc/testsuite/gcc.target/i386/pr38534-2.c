/* { dg-do compile } */
/* { dg-options "-O2 -mtune-ctrl=^prologue_using_move,^epilogue_using_move -fomit-frame-pointer" } */

extern void bar (void) __attribute__ ((no_callee_saved_registers));
extern void fn (void) __attribute__ ((noreturn));

__attribute__ ((noreturn))
void
foo (void)
{
  bar ();
  fn ();
}

/* { dg-final { scan-assembler-not "push" } } */
/* { dg-final { scan-assembler-not "pop" } } */
/* { dg-final { scan-assembler-not "jmp\[\\t \]+_?bar" } } */
/* { dg-final { scan-assembler "call\[\\t \]+_?bar" } } */
