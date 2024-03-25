/* Test for unified syntax assembly generation.  */
/* { dg-do compile } */
/* { dg-require-effective-target arm_arch_v7a_ok } */
/* { dg-options "-marm -masm-syntax-unified" } */
/* { dg-add-options arm_arch_v7a } */

void test ()
{
  asm("nop");
}

/* { dg-final { scan-assembler-times {\.syntax\sunified} 3 } } */
/* { dg-final { scan-assembler-not {\.syntax\sdivided} } } */

