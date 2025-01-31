/* PR ipa/117432 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler "# myinsn %rax" } } */
/* { dg-final { scan-assembler "# myinsn %eax" } } */

void
foo (void)
{
  asm volatile ("# myinsn %0" : : "r" (-42L));
}

void
bar (void)
{
  asm volatile ("# myinsn %0" : : "r" (-42));
}
