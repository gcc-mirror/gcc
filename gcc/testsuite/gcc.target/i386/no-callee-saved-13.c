/* { dg-do compile } */
/* { dg-options "-O2 -mtune-ctrl=^prologue_using_move,^epilogue_using_move" } */

extern void foo (void);

__attribute__ ((no_callee_saved_registers))
void
bar (void)
{
  foo ();
}

/* { dg-final { scan-assembler-not "push" } } */
/* { dg-final { scan-assembler-not "pop" } } */
/* { dg-final { scan-assembler-not "call\[\\t \]+_?foo" } } */
/* { dg-final { scan-assembler "jmp\[\\t \]+_?foo" } } */
