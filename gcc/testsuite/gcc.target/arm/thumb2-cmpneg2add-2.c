/* Use ADDS with a scratch, rather than CMN */
/* { dg-options "-mthumb -Os" } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-final { scan-assembler "adds" } } */
/* { dg-final { scan-assembler-not "cmn" } } */

void foo1(int);
void bar5(int x)
{
  if (x == -1)
    foo1(x);
}
