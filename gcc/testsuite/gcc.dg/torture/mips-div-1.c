/* Check that hard-float MIPS code doesn't use library calls for 1.0/x.  */
/* { dg-options "-fno-delayed-branch" } */
/* { dg-do compile { target mips*-*-* } } */

float f1 (float x) { return 1.0f / x; }
double f2 (double x) { return 1.0 / x; }

/* { dg-final { scan-assembler-not {lwc1.*__divsf3} } } */
/* { dg-final { scan-assembler-not {ldc1.*__divdf3} } } */
