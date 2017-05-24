/* { dg-do compile } */
/* { dg-skip-if "Not available for ARCv1" { arc700 || arc6xx } } */
/* { dg-options "-O2 -mirq-ctrl-saved=r0-r17" } */

/* Check if the registers R0-R17 are automatically saved.  GP is saved
   by the compiler.  */

int a;

void  __attribute__ ((interrupt("ilink")))
foo(void)
{
  __asm__ volatile ( "" : : : "r0","r1","r2","r3");
  __asm__ volatile ( "" : : : "r13","r14","r15","r16");
  a++;
}
/* { dg-final { scan-assembler-not "st.*r13,\\\[sp" } } */
/* { dg-final { scan-assembler-not "st.*r14,\\\[sp" } } */
/* { dg-final { scan-assembler-not "st.*r15,\\\[sp" } } */
/* { dg-final { scan-assembler-not "st.*r16,\\\[sp" } } */
/* { dg-final { scan-assembler "st.*gp,\\\[sp,-4\\\]" } } */
/* { dg-final { scan-assembler "ld.*gp,\\\[sp\\\]" } } */
/* { dg-final { scan-assembler-not "st.*r0,\\\[sp" } } */
/* { dg-final { scan-assembler-not "st.*r1,\\\[sp" } } */
/* { dg-final { scan-assembler-not "st.*r2,\\\[sp" } } */
/* { dg-final { scan-assembler-not "st.*r3,\\\[sp" } } */
/* { dg-final { scan-assembler "rtie" } } */
