/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O2 -ftree-vectorize" } */
/* { dg-add-options arm_neon } */
/* { dg-final { scan-assembler "vshr\.u32.*#3" } } */

/* Verify that VSHR immediate is used.  */
void f1(int n, unsigned int x[], unsigned int y[]) {
  int i;
  for (i = 0; i < n; ++i)
    y[i] = x[i] >> 3;
}
