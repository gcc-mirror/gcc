/* Verify that -mno-fancy-math-387 works.  */
/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-options "-O -ffast-math -mfpmath=387 -mno-fancy-math-387 -march=i386" } */
/* { dg-final { scan-assembler "call\t_?atan" } } */

double f1(double x) { return __builtin_atan(x); }
