/* Make sure that mul/addu is preferred over mtlo/macc and that mul/subu
   is preferred over mtlo/msac.  */
/* { dg-do compile } */
/* { dg-options "-march=vr5400 -fpeephole2" } */
/* { dg-skip-if "code quality test" { *-*-* } { "-O0" } { "" } } */
NOMIPS16 int f1 (int a, int b, int c) { return a + b * c; }
NOMIPS16 int f2 (int a, int b, int c) { return a - b * c; }
/* { dg-final { scan-assembler "\tmul\t.*\tmul\t" } } */
