/* { dg-options "-mthumb -O2" }  */
/* { dg-require-effective-target arm_thumb1_ok } */
/* { dg-final { scan-assembler-not "ldr" } } */

float foo (void)
{
  return 2.0;
}
