/* Verify that -mno-fancy-math-387 works.  */
/* { dg-do compile { target "i?86-*-*" } } */
/* { dg-skip-if "" { i?86-*-* } { "-m64" } { "" } } */
/* { dg-options "-O -ffast-math -mfpmath=387 -mno-fancy-math-387 -march=i386" } */
/* { dg-final { scan-assembler "call\t_?sin" } } */
/* { dg-final { scan-assembler "call\t_?cos" } } */
/* { dg-final { scan-assembler "call\t_?sqrt" } } */
/* { dg-final { scan-assembler "call\t_?atan2" } } */
/* { dg-final { scan-assembler "call\t_?log" } } */
/* { dg-final { scan-assembler "call\t_?exp" } } */
/* { dg-final { scan-assembler "call\t_?tan" } } */
/* { dg-final { scan-assembler "call\t_?fmod" } } */

double f1(double x) { return __builtin_sin(x); }
double f2(double x) { return __builtin_cos(x); }
double f3(double x) { return __builtin_sqrt(x); }
double f4(double x, double y) { return __builtin_atan2(x,y); }
double f5(double x) { return __builtin_log(x); }
double f6(double x) { return __builtin_exp(x); }
double f7(double x) { return __builtin_tan(x); }
double f8(double x, double y) { return __builtin_fmod(x,y); }
