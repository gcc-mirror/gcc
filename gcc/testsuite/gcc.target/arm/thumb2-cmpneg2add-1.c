/* Use ADDS clobbering source operand, rather than CMN */
/* { dg-options "-mthumb -Os" } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-final { scan-assembler "adds" } } */
/* { dg-final { scan-assembler-not "cmn" } } */

void foo1(void);
void bar5(int x)
{
  if (x == -15)
    foo1();
}
