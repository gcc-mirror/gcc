/* Test that ifcvt is not being too aggressive when -mrestrict-it.  */
/* { dg-do compile } */
/* { dg-options "-O2 -mrestrict-it" } */
/* { dg-require-effective-target arm_thumb2_ok } */

int
f1(int x, int y, int z)
{
  if (x > 100)
    {
      x++;
      z = -z;
    }
  else
    {
      x = -x;
      y = -y;
    }
  return x + y + z;
}
/* { dg-final { scan-assembler "b(gt|le)" } } */
