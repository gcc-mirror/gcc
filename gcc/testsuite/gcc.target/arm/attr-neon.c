/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O2 -mfloat-abi=softfp -ftree-vectorize" } */

/* Verify that neon instructions are emitted once.  */
void __attribute__ ((target("fpu=neon")))
 f1(int n, int x[], int y[]) {
  int i;
  for (i = 0; i < n; ++i)
    y[i] = x[i] << 3;
}

void __attribute__ ((target("fpu=vfp")))
f3(int n, int x[], int y[]) {
  int i;
  for (i = 0; i < n; ++i)
    y[i] = x[i] << 3;
}

/* { dg-final { scan-assembler-times "\.fpu vfp" 1 } } */
/* { dg-final { scan-assembler-times "\.fpu neon" 1 } } */
/* { dg-final { scan-assembler-times "vshl" 1 } } */
