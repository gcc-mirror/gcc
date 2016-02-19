/* Checks proper behavior of __morestack function - specifically, GPR
   values surviving, stack parameters being copied, and vararg
   pointer being correct.  */

/* { dg-do run } */
/* { dg-options "" } */

#include <stdlib.h>

void *orig_r15;

/* 1. Function "test" saves registers, makes a stack frame, puts known
 *    values in registers, and calls __morestack, telling it to jump to
 *    testinner, with return address pointing to "testret".
 * 2. "testinner" checks that parameter registers match what has been
 *    passed from "test", stack parameters were copied properly to
 *    the new stack, and the argument pointer matches the calling
 *    function's stack pointer.  It then leaves new values in volatile
 *    registers (including return value registers) and returns.
 * 3. "testret" checks that return value registers contain the expected
 *    return value, callee-saved GPRs match the values from "test",
 *    and then returns to main. */

extern unsigned long testparams[3];

#ifdef __s390x__

asm(
  ".global test\n"
  "test:\n"
  ".type test, @function\n"
  /* Save registers.  */
  "stmg %r6, %r15, 0x30(%r15)\n"
  /* Save original sp in a global.  */
  "larl %r1, orig_r15\n"
  "stg %r15, 0(%r1)\n"
  /* Make a stack frame.  */
  "aghi %r15, -168\n"
  /* A stack parameter.  */
  "lghi %r1, 0x1240\n"
  "stg %r1, 160(%r15)\n"
  /* Registers.  */
  "lghi %r0, 0x1230\n"
  "lghi %r2, 0x1232\n"
  "lghi %r3, 0x1233\n"
  "lghi %r4, 0x1234\n"
  "lghi %r5, 0x1235\n"
  "lghi %r6, 0x1236\n"
  "lghi %r7, 0x1237\n"
  "lghi %r8, 0x1238\n"
  "lghi %r9, 0x1239\n"
  "lghi %r10, 0x123a\n"
  "lghi %r11, 0x123b\n"
  "lghi %r12, 0x123c\n"
  "lghi %r13, 0x123d\n"
  /* Fake return address.  */
  "larl %r14, testret\n"
  /* Call morestack.  */
  "larl %r1, testparams\n"
  "jg __morestack\n"

  /* Entry point.  */
  "testinner:\n"
  /* Check registers.  */
  "cghi %r0, 0x1230\n"
  "jne testerr\n"
  "cghi %r2, 0x1232\n"
  "jne testerr\n"
  "cghi %r3, 0x1233\n"
  "jne testerr\n"
  "cghi %r4, 0x1234\n"
  "jne testerr\n"
  "cghi %r5, 0x1235\n"
  "jne testerr\n"
  "cghi %r6, 0x1236\n"
  "jne testerr\n"
  /* Check stack param.  */
  "lg %r0, 0xa0(%r15)\n"
  "cghi %r0, 0x1240\n"
  "jne testerr\n"
  /* Check argument pointer.  */
  "aghi %r1, 8\n"
  "larl %r2, orig_r15\n"
  "cg %r1, 0(%r2)\n"
  "jne testerr\n"
  /* Modify volatile registers.  */
  "lghi %r0, 0x1250\n"
  "lghi %r1, 0x1251\n"
  "lghi %r2, 0x1252\n"
  "lghi %r3, 0x1253\n"
  "lghi %r4, 0x1254\n"
  "lghi %r5, 0x1255\n"
  /* Return.  */
  "br %r14\n"

  /* Returns here.  */
  "testret:\n"
  /* Check return registers.  */
  "cghi %r2, 0x1252\n"
  "jne testerr\n"
  /* Check callee-saved registers.  */
  "cghi %r6, 0x1236\n"
  "jne testerr\n"
  "cghi %r7, 0x1237\n"
  "jne testerr\n"
  "cghi %r8, 0x1238\n"
  "jne testerr\n"
  "cghi %r9, 0x1239\n"
  "jne testerr\n"
  "cghi %r10, 0x123a\n"
  "jne testerr\n"
  "cghi %r11, 0x123b\n"
  "jne testerr\n"
  "cghi %r12, 0x123c\n"
  "jne testerr\n"
  "cghi %r13, 0x123d\n"
  "jne testerr\n"
  /* Return.  */
  "lmg %r6, %r15, 0xd8(%r15)\n"
  "br %r14\n" 

  /* Parameters block.  */
  ".section .data\n"
  ".align 8\n"
  "testparams:\n"
  ".quad 160\n"
  ".quad 8\n"
  ".quad testinner-testparams\n"
  ".text\n"
);

#else

asm(
  ".global test\n"
  "test:\n"
  ".type test, @function\n"
  /* Save registers.  */
  "stm %r6, %r15, 0x18(%r15)\n"
  /* Save original sp in a global.  */
  "larl %r1, orig_r15\n"
  "st %r15, 0(%r1)\n"
  /* Make a stack frame.  */
  "ahi %r15, -0x68\n"
  /* A stack parameter.  */
  "lhi %r1, 0x1240\n"
  "st %r1, 0x60(%r15)\n"
  "lhi %r1, 0x1241\n"
  "st %r1, 0x64(%r15)\n"
  /* Registers.  */
  "lhi %r0, 0x1230\n"
  "lhi %r2, 0x1232\n"
  "lhi %r3, 0x1233\n"
  "lhi %r4, 0x1234\n"
  "lhi %r5, 0x1235\n"
  "lhi %r6, 0x1236\n"
  "lhi %r7, 0x1237\n"
  "lhi %r8, 0x1238\n"
  "lhi %r9, 0x1239\n"
  "lhi %r10, 0x123a\n"
  "lhi %r11, 0x123b\n"
  "lhi %r12, 0x123c\n"
  "lhi %r13, 0x123d\n"
  /* Fake return address.  */
  "larl %r14, testret\n"
  /* Call morestack.  */
  "larl %r1, testparams\n"
  "jg __morestack\n"

  /* Entry point.  */
  "testinner:\n"
  /* Check registers.  */
  "chi %r0, 0x1230\n"
  "jne testerr\n"
  "chi %r2, 0x1232\n"
  "jne testerr\n"
  "chi %r3, 0x1233\n"
  "jne testerr\n"
  "chi %r4, 0x1234\n"
  "jne testerr\n"
  "chi %r5, 0x1235\n"
  "jne testerr\n"
  "chi %r6, 0x1236\n"
  "jne testerr\n"
  /* Check stack param.  */
  "l %r0, 0x60(%r15)\n"
  "chi %r0, 0x1240\n"
  "jne testerr\n"
  "l %r0, 0x64(%r15)\n"
  "chi %r0, 0x1241\n"
  "jne testerr\n"
  /* Check argument pointer.  */
  "ahi %r1, 8\n"
  "larl %r2, orig_r15\n"
  "c %r1, 0(%r2)\n"
  "jne testerr\n"
  /* Modify volatile registers.  */
  "lhi %r0, 0x1250\n"
  "lhi %r1, 0x1251\n"
  "lhi %r2, 0x1252\n"
  "lhi %r3, 0x1253\n"
  "lhi %r4, 0x1254\n"
  "lhi %r5, 0x1255\n"
  /* Return.  */
  "br %r14\n"

  /* Returns here.  */
  "testret:\n"
  /* Check return registers.  */
  "chi %r2, 0x1252\n"
  "jne testerr\n"
  "chi %r3, 0x1253\n"
  "jne testerr\n"
  /* Check callee-saved registers.  */
  "chi %r6, 0x1236\n"
  "jne testerr\n"
  "chi %r7, 0x1237\n"
  "jne testerr\n"
  "chi %r8, 0x1238\n"
  "jne testerr\n"
  "chi %r9, 0x1239\n"
  "jne testerr\n"
  "chi %r10, 0x123a\n"
  "jne testerr\n"
  "chi %r11, 0x123b\n"
  "jne testerr\n"
  "chi %r12, 0x123c\n"
  "jne testerr\n"
  "chi %r13, 0x123d\n"
  "jne testerr\n"
  /* Return.  */
  "lm %r6, %r15, 0x80(%r15)\n"
  "br %r14\n" 

  /* Parameters block.  */
  ".section .data\n"
  ".align 4\n"
  "testparams:\n"
  ".long 96\n"
  ".long 8\n"
  ".long testinner-testparams\n"
  ".text\n"
);

#endif

_Noreturn void testerr (void) {
  exit(1);
}

extern void test (void);

int main (void) {
  test();
  /* Now try again, with huge stack frame requested - to exercise
     both paths in __morestack (new allocation needed or not).  */
  testparams[0] = 1000000;
  test();
  return 0;
}
