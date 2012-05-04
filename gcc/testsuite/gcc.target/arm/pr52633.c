/* PR tree-optimization/52633 */
/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-march=armv7-a -mfloat-abi=softfp -mfpu=neon -O -ftree-vectorize" } */

void
test (unsigned short *x, signed char *y)
{
  int i;
  for (i = 0; i < 32; i++)
    x[i] = (short) (y[i] << 5);
}

