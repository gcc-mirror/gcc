/* { dg-do compile } */
/* { dg-require-effective-target arm_fp_ok } */
/* { dg-skip-if "need fp instructions" { *-*-* } { "-mfloat-abi=soft" } { "" } } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_fp } */

void
foo (float *p, float a, float b, int n)
{
  float c = a + b;
  do
    *--p = c;
  while (n--);
}

/* { dg-final { scan-assembler "vstmdb.32" } } */
