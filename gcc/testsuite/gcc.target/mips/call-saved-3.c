/* Check that we save all call-saved GPRs in a MIPS16 __builtin_setjmp
   function.  */
/* { dg-options "(-mips16) isa_rev=0" } */

void bar (void);
extern int buf[];

MIPS16 void
foo (int x)
{
  if (__builtin_setjmp (buf) == 0)
    bar();
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
