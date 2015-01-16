/* { dg-do compile } */
/* { dg-options "-mthumb -Os " }  */
/* { dg-require-effective-target arm_thumb1_ok } */

void save_regs () {
  __asm volatile ("" ::: "r8");
}

/* { dg-final { scan-assembler "\tmov\tr., r8" } } */
