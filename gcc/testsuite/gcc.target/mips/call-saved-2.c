/* Check that we save non-MIPS16 GPRs if they are explicitly clobbered.  */
/* { dg-do compile { target mips16_attribute } } */
/* { dg-mips-options "-mips2 -O2" } */
/* { dg-add-options mips16_attribute } */

MIPS16 void
foo (void)
{
  asm volatile ("" ::: "$19", "$23", "$24", "$30");
}
/* { dg-final { scan-assembler-not "\\\$16" } } */
/* { dg-final { scan-assembler-not "\\\$17" } } */
/* { dg-final { scan-assembler-not "\\\$18" } } */
/* { dg-final { scan-assembler "\\\$19" } } */
/* { dg-final { scan-assembler-not "\\\$20" } } */
/* { dg-final { scan-assembler-not "\\\$21" } } */
/* { dg-final { scan-assembler-not "\\\$22" } } */
/* { dg-final { scan-assembler "\\\$23" } } */
/* { dg-final { scan-assembler-not "\\\$24" } } */
/* { dg-final { scan-assembler "\\\$(30|fp)" } } */
