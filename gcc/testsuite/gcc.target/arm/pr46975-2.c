/* { dg-options "-mthumb -O2" } */
/* { dg-require-effective-target arm_thumb2_ok } */
/* { dg-final { scan-assembler "sub" } } */
/* { dg-final { scan-assembler "clz" } } */
/* { dg-final { scan-assembler "lsr.*#5" } } */

int foo (int s)
{
      return s == 1;
}
