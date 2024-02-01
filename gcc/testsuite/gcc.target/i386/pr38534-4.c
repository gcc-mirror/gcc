/* { dg-do compile } */
/* { dg-options "-O2 -mtune-ctrl=^prologue_using_move,^epilogue_using_move -fomit-frame-pointer" } */

typedef void (*fn_t) (void) __attribute__ ((no_callee_saved_registers));
extern void fn (void) __attribute__ ((noreturn));

__attribute__ ((noreturn))
void
foo (fn_t bar)
{
  bar ();
  fn ();
}

/* { dg-final { scan-assembler-not "push" } } */
/* { dg-final { scan-assembler-not "pop" } } */
/* { dg-final { scan-assembler-not "jmp" } } */
/* { dg-final { scan-assembler "call\[\\t \]+" } } */
