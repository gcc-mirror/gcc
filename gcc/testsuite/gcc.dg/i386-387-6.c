/* Verify that -march overrides -mno-fancy-math-387.  */
/* { dg-do compile { target "i?86-*-*" } } */
/* { dg-options "-O -ffast-math -mfpmath=387 -march=i686 -mno-fancy-math-387" } */
/* { dg-final { scan-assembler "fpatan" } } */

double f1(double x) { return __builtin_atan(x); }
