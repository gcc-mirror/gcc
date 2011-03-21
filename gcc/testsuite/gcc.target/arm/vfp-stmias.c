/* { dg-do compile } */
/* { dg-require-effective-target arm_vfp_ok } */
/* { dg-options "-O2 -mfpu=vfp -mfloat-abi=softfp" } */

void
foo (float *p, float a, float b, int n)
{
  float c = a + b;
  do
    *p++ = c;
  while (n--);
}

/* { dg-final { scan-assembler "fstmias" } } */
