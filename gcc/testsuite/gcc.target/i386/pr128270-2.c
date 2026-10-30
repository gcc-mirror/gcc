/* PR target/128270 */
/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -mforce-drap -mtune-ctrl=^use_leave -fasynchronous-unwind-tables -masm=att" } */

void use (void *);

int
f (void)
{
  char buf[64] __attribute__ ((aligned (32)));
  use (buf);
  return buf[0];
}

/* { dg-final { scan-assembler "\[ \t\]popq\t%rbp" } } */
/* Same as pr128270-1.c, but exercising the "pop %rbp" epilogue path.  */
/* { dg-final { scan-assembler "\\.cfi_restore 6" } } */
