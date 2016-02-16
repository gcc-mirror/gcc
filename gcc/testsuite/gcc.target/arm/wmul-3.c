/* { dg-do compile } */
/* { dg-require-effective-target arm_dsp } */
/* { dg-options "-O" } */

int
foo (int a, int b)
{
  return (short) a * (short) b;
}

/* { dg-final { scan-assembler-times "smulbb" 1 } } */
