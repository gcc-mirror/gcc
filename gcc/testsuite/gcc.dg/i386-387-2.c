/* Verify that -march overrides -mno-fancy-math-387.  */
/* { dg-do compile { target "i?86-*-*" } } */
/* { dg-options "-O -ffast-math -mfpmath=387 -march=i686 -mno-fancy-math-387" } */
/* { dg-final { scan-assembler "fsin" } } */
/* { dg-final { scan-assembler "fcos" } } */
/* { dg-final { scan-assembler "fsqrt" } } */

double f1(double x) { return __builtin_sin(x); }
double f2(double x) { return __builtin_cos(x); }
double f3(double x) { return __builtin_sqrt(x); }
