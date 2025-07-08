/* { dg-do compile } */
/* { dg-options "-O2 -fstack-protector-all" } */
/* { dg-final { scan-assembler-times {\tear\t%r[0-9]+,%a[01]} 8 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\tsllg\t%r[0-9]+,%r[0-9]+,32} 4 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\tear\t%r[0-9]+,%a[01]} 3 { target { ! lp64 } } } } */
/* { dg-final { scan-assembler-times {\tmvc\t160\(8,%r15\),40\(%r[0-9]+\)} 2 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\tmvc\t100\(4,%r15\),20\(%r[0-9]+\)} 2 { target { ! lp64 } } } } */
/* { dg-final { scan-assembler-times {\tclc\t160\(8,%r15\),40\(%r[0-9]+\)} 2 { target lp64 } } } */
/* { dg-final { scan-assembler-times {\tclc\t100\(4,%r15\),20\(%r[0-9]+\)} 2 { target { ! lp64 } } } } */

/* Computing the address of the thread pointer on s390 involves multiple
   instructions and therefore bears the risk that the address of the canary or
   intermediate values of it are spilled and reloaded.  Therefore, as a
   precaution compute the address always twice, i.e., one time for the prologue
   and one time for the epilogue.  */

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
