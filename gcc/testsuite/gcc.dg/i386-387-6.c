/* Verify that -march overrides -mno-fancy-math-387.  */
/* { dg-do compile { target "i?86-*-*" } } */
/* { dg-skip-if "" { i?86-*-* } { "-m64" } { "" } } */
/* { dg-options "-O -ffast-math -mfpmath=387 -march=i686 -mno-fancy-math-387" } */
/* { dg-final { scan-assembler "fpatan" } } */
/* { dg-final { scan-assembler "fyl2xp1" } } */
/* { dg-final { scan-assembler "fprem1" } } */

double f1(double x) { return __builtin_atan(x); }
double f2(double x) { return __builtin_log1p(x); }
double f3(double x, double y) { return __builtin_drem(x,y); }
