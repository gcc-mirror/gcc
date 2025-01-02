/* { dg-do compile { target fstack_protector } } */
/* { dg-options "-O2 -fstack-protector-all" } */
/* { dg-final { scan-assembler-not "__stack_chk_fail" } } */

__attribute__ ((naked))
void
foo (void)
{
  asm ("ret");
}
