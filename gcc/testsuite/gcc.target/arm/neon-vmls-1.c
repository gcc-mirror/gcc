/* { dg-require-effective-target arm_neon_hw } */
/* { dg-options "-O2 -ftree-vectorize -ffast-math" } */
/* { dg-add-options arm_neon } */
/* { dg-final { scan-assembler "vmls\\.i32" } } */

/* Verify that VMLS is used.  */
void f1(int n, int a, int x[], int y[]) {
  int i;
  for (i = 0; i < n; ++i)
    y[i] = y[i] - a * x[i];
}
