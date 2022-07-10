/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2" } */
/* { dg-final { scan-assembler-not "movq" } } */
/* { dg-final { scan-assembler "cvtps2pd" } } */

void
foo (float a[2], double b[2])
{
    int i;
    for (i = 0; i < 2; i++)
      b[i] = a[i];
}
