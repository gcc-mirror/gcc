/* PR target/128270 */
/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -mforce-drap -mtune-ctrl=use_leave -fasynchronous-unwind-tables" } */

void use (void *);

int
f (void)
{
  char buf[64] __attribute__ ((aligned (32)));
  use (buf);
  return buf[0];
}

/* { dg-final { scan-assembler "\[ \t\]leave" } } */
/* The DRAP prologue saves %rbp with an FP-relative DW_CFA_expression
   rule, so the epilogue must restore it even within the red zone.  */
/* { dg-final { scan-assembler "\\.cfi_restore 6" } } */
