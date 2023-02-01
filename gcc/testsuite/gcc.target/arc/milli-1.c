/* { dg-do compile } */
/* { dg-options "-Os -mmillicode" } */

/* Test if we restore correctly blink when using millicode.  */
extern void bar (void);

void foo (void)
{
  __asm__ volatile ( "" : : : "r13","r14","r15","r16","r17","r18","r20","r21");
  bar();
}

void foo2 (void)
{
  bar();
  __asm__ volatile ( "" : : : "r13","r14","r15","r16","r17","r18","r20","r21");
}

/* { dg-final { scan-assembler-not "st.*r13,\\\[sp" } } */
/* { dg-final { scan-assembler-not "st.*r14,\\\[sp" } } */
/* { dg-final { scan-assembler-not "st.*r15,\\\[sp" } } */
/* { dg-final { scan-assembler "ld.*blink,\\\[sp,32" } } */
/* { dg-final { scan-assembler "mov_s.*r12,32" } } */
