/* Verify that -mno-fancy-math-387 works.  */
/* { dg-do compile { target "i?86-*-*" } } */
/* { dg-options "-O -ffast-math -mfpmath=387 -mno-fancy-math-387" } */
/* { dg-final { scan-assembler "call\tsin" } } */
/* { dg-final { scan-assembler "call\tcos" } } */
/* { dg-final { scan-assembler "call\tsqrt" } } */

double f1(double x) { return __builtin_sin(x); }
double f2(double x) { return __builtin_cos(x); }
double f3(double x) { return __builtin_sqrt(x); }
