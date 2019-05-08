/* { dg-do compile } */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-options "-O2 -mthumb" }  */

void *retaddr;

void foo (void) {
  retaddr = __builtin_return_address (0);

  /* Used for enforcing registers stacking.  */
  asm volatile ("" : : : "r0", "r1", "r2", "r3", "r4", "r5", "r6", "r7",
			 "r8", "r9", "r10", "r11", "r12");
}

/* { dg-final { scan-assembler-not "mov\tlr," } } */
