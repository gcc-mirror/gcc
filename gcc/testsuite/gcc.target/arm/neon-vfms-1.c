/* { dg-do compile } */
/* { dg-require-effective-target arm_neonv2_ok } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math" } */
/* { dg-add-options arm_neonv2 } */
/* { dg-final { scan-assembler "vfms\\.f32\[	\]+\[dDqQ]" } } */

/* Verify that VFMS is used.  */
void f1(int n, float a, float x[], float y[]) {
  int i;
  for (i = 0; i < n; ++i)
    y[i] = a * -x[i] + y[i];
}
