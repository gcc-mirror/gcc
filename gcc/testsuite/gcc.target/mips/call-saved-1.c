/* Check that we save all call-saved GPRs in a MIPS16 __builtin_eh_return
   function.  */
/* { dg-do compile { target mips16_attribute } } */
/* { dg-mips-options "-mips2" } */
/* { dg-add-options mips16_attribute } */

void bar (void);

MIPS16 void
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
