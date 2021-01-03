/* PR target/97887 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mfpmath=sse" } */

float f (float a)
{
  return -a / a;
}

double d (double a)
{
  return -a / a;
}

/* { dg-final { scan-assembler-not "fchs" } } */
