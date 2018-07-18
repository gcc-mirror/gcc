/* { dg-do compile } */
/* { dg-skip-if "Not available for ARCv1" { arc700 ||  arc6xx } } */
/* { dg-options "-O2 -mirq-ctrl-saved=r0-r17,blink" } */

/* Check if the registers R0-R17,blink are automatically saved. */

void  __attribute__ ((interrupt("ilink")))
foo(void)
{
  __asm__ volatile ( "" : : : "r13","r14","r15","r16");
}
/* { dg-final { scan-assembler-not "st.*r13,\\\[sp" } } */
/* { dg-final { scan-assembler-not "st.*r14,\\\[sp" } } */
/* { dg-final { scan-assembler-not "st.*r15,\\\[sp" } } */
/* { dg-final { scan-assembler-not "st.*r16,\\\[sp" } } */
/* { dg-final { scan-assembler-not "push_s blink" } } */
