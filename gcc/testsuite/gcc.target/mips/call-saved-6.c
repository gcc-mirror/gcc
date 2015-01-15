/* Check that we save the correct call-saved GPRs and FPRs.  */
/* { dg-options "-mabi=32 -mfp64" } */

void bar (void);

NOCOMPRESSION void
foo (int x)
{
  __builtin_unwind_init ();
  __builtin_eh_return (x, bar);
}
/* { dg-final { scan-assembler "\\\$16" } } */
/* { dg-final { scan-assembler "\\\$17" } } */
/* { dg-final { scan-assembler "\\\$18" } } */
/* { dg-final { scan-assembler "\\\$19" } } */
/* { dg-final { scan-assembler "\\\$20" } } */
/* { dg-final { scan-assembler "\\\$21" } } */
/* { dg-final { scan-assembler "\\\$22" } } */
/* { dg-final { scan-assembler "\\\$23" } } */
/* { dg-final { scan-assembler "\\\$(30|fp)" } } */
/* { dg-final { scan-assembler "\\\$f20" } } */
/* { dg-final { scan-assembler "\\\$f22" } } */
/* { dg-final { scan-assembler "\\\$f24" } } */
/* { dg-final { scan-assembler "\\\$f26" } } */
/* { dg-final { scan-assembler "\\\$f28" } } */
/* { dg-final { scan-assembler "\\\$f30" } } */
/* { dg-final { scan-assembler-not "\\\$f21" } } */
/* { dg-final { scan-assembler-not "\\\$f23" } } */
/* { dg-final { scan-assembler-not "\\\$f25" } } */
/* { dg-final { scan-assembler-not "\\\$f27" } } */
/* { dg-final { scan-assembler-not "\\\$f29" } } */
/* { dg-final { scan-assembler-not "\\\$f31" } } */
