/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-O2 -march=x86-64 -mavx10.2 -mapxf" } */
/* { dg-final { scan-assembler-times "fucomip" 2 } } */
/* { dg-final { scan-assembler-not "comx" } } */

int f1 (long double a, long double b, int x) { return x > 0 && a == b; }
int f2 (long double a, long double b, int x) { return x > 0 && a != b; }
