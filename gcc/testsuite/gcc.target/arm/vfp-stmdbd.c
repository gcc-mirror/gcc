/* { dg-do compile } */
/* { dg-require-effective-target arm_vfp_ok } */
/* { dg-options "-O2 -mfpu=vfp -mfloat-abi=softfp" } */

void
foo (double *p, double a, double b, int n)
{
  double c = a + b;
  do
    *--p = c;
  while (n--);
}

/* { dg-final { scan-assembler "fstmdbd" } } */
