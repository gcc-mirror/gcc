/* PR target/52375 */
/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-march=armv7-a -mfloat-abi=softfp -mfpu=neon -O -ftree-vectorize" } */

unsigned int output[4];

void test (unsigned short *p)
{
  unsigned int x = *p;
  if (x)
    {
      output[0] = x << 1;
      output[1] = x << 1;
      output[2] = x << 1;
      output[3] = x << 1;
    }
}

