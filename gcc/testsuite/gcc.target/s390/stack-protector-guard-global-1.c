/* { dg-do compile } */
/* { dg-options "-O2 -fstack-protector-all -mstack-protector-guard=global -mstack-protector-guard-record" } */
/* { dg-final { scan-assembler-times {\n1:\n\tlarl\t%r[0-9]+,__stack_chk_guard\n} 4 } } */

void test_0 (void) { }

void test_1 (void)
{
  __asm__ __volatile ("" :::
      "r0",
      "r1",
      "r2",
      "r3",
      "r4",
      "r5",
      "r6",
      "r7",
      "r8",
      "r9",
      "r10",
      "r11",
#ifndef __PIC__
      "r12",
#endif
      "r13",
      "r14");
}
