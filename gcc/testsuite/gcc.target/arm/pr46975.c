/* { dg-options "-mthumb -Os" } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-final { scan-assembler "subs" } } */
/* { dg-final { scan-assembler "adcs" } } */

int foo (int s)
{
      return s == 1;
}
