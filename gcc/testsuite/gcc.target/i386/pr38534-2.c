/* { dg-do compile } */
/* { dg-options "-O2 -mtune-ctrl=^prologue_using_move,^epilogue_using_move -fomit-frame-pointer -mnoreturn-no-callee-saved-registers" } */

extern void bar (void) __attribute__ ((no_callee_saved_registers));
extern void fn (void) __attribute__ ((noreturn));

__attribute__ ((noreturn))
void
foo (void)
{
  bar ();
  fn ();
}

/* { dg-final { scan-assembler-not "push\[^\n\r\]*(?:\[abcd\]x|\[sd\]i|sp|r\[0-9\]|\[xyz\]mm)" } } */
/* { dg-final { scan-assembler-not "pop\[^\n\r\]*(?:\[abcd\]x|\[sd\]i|sp|r\[0-9\]|\[xyz\]mm)" } } */
/* { dg-final { scan-assembler-not "jmp\[\\t \]+_?bar" } } */
/* { dg-final { scan-assembler "call\[\\t \]+_?bar" } } */
