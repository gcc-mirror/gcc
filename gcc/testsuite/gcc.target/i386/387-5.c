/* Verify that -mno-fancy-math-387 works.  */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O -ffast-math -mfpmath=387 -mno-fancy-math-387 -march=i386" } */
/* { dg-final { scan-assembler "call\t_?atan" } } */
/* { dg-final { scan-assembler "call\t_?log1p" } } */
/* { dg-final { scan-assembler "call\t_?drem" } } */

double f1(double x) { return __builtin_atan(x); }
double f2(double x) { return __builtin_log1p(x); }
double f3(double x, double y) { return __builtin_drem(x,y); }
