/* { dg-do compile } */
/* { dg-options "-O2 -mtune-ctrl=^prologue_using_move,^epilogue_using_move" } */

typedef void (*fn_t) (void) __attribute__ ((no_callee_saved_registers));

__attribute__ ((no_callee_saved_registers))
void
foo (fn_t bar)
{
  bar ();
}

/* { dg-final { scan-assembler-not "push" } } */
/* { dg-final { scan-assembler-not "pop" } } */
/* { dg-final { scan-assembler "jmp" } } */
/* { dg-final { scan-assembler-not "call\[\\t \]+" } } */
