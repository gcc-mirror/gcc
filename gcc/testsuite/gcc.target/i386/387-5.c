/* Verify that -mno-fancy-math-387 works.  */
/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-skip-if "" { *-*-* } { "-march=*" } { "-march=i386" } } */
/* { dg-options "-O -ffast-math -mfpmath=387 -mno-fancy-math-387 -march=i386" } */
/* { dg-final { scan-assembler "call\t(.*)atan" } } */
/* { dg-final { scan-assembler "call\t(.*)log1p" } } */
/* { dg-final { scan-assembler "call\t(.*)drem" } } */

double f1(double x) { return __builtin_atan(x); }
double f2(double x) { return __builtin_log1p(x); }
double f3(double x, double y) { return __builtin_drem(x,y); }
