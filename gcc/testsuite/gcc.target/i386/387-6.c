/* Verify that -march overrides -mno-fancy-math-387.  */
/* { dg-do compile } */
/* { dg-require-effective-target ia32 } */
/* { dg-skip-if "" { *-*-* } { "-march=*" } { "-march=i686" } } */
/* { dg-options "-O -ffast-math -mfpmath=387 -march=i686 -mno-fancy-math-387" } */
/* { dg-final { scan-assembler "fpatan" } } */
/* { dg-final { scan-assembler "fyl2xp1" } } */
/* { dg-final { scan-assembler "fprem1" } } */

double f1(double x) { return __builtin_atan(x); }
double f2(double x) { return __builtin_log1p(x); }
double f3(double x, double y) { return __builtin_drem(x,y); }
