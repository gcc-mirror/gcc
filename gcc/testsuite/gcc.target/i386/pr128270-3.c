/* PR target/128270 */
/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -mforce-drap -mtune-ctrl=use_leave,prologue_using_move,epilogue_using_move -fasynchronous-unwind-tables" } */

void use (void *);

int
f (void)
{
  char buf[64] __attribute__ ((aligned (32)));
  asm volatile ("" ::: "rbx", "r12");
  use (buf);
  return buf[0];
}

/* { dg-final { scan-assembler "\[ \t\]leave" } } */
/* Registers saved by moves in a DRAP frame use FP-relative expression
   rules, so they must be restored after "leave" as well.  */
/* { dg-final { scan-assembler "\\.cfi_restore 3" } } */
/* { dg-final { scan-assembler "\\.cfi_restore 12" } } */
/* { dg-final { scan-assembler "\\.cfi_restore 6" } } */
