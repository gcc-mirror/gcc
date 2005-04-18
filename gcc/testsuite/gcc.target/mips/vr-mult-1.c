/* Make sure that mul/addu is preferred over mtlo/macc and that mul/subu
   is preferred over mtlo/msac.  */
/* { dg-do compile } */
/* { dg-mips-options "-O2 -march=vr5400" } */
int f1 (int a, int b, int c) { return a + b * c; }
int f2 (int a, int b, int c) { return a - b * c; }
/* { dg-final { scan-assembler "\tmul\t.*\tmul\t" } } */
