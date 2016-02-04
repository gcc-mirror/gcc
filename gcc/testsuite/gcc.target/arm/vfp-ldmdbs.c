/* { dg-do compile } */
/* { dg-require-effective-target arm_fp_ok } */
/* { dg-skip-if "need fp instructions" { *-*-* } { "-mfloat-abi=soft" } { "" } } */
/* { dg-options "-O2" } */
/* { dg-add-options arm_fp } */

extern void bar (float);

void
foo (float *p, float a, int n)
{
  do
    bar (*--p + a);
  while (n--);
}

/* { dg-final { scan-assembler "vldmdb.32" } } */
