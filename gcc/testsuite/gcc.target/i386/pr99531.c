/* { dg-do compile { target { x86_64-*-linux* } } } */
/* { dg-options "-O2" } */

int func(int, int, int, int, int, int);
int caller(int a, int b, int c, int d, int e) { return func(0, a, b, c, d, e); }

/* { dg-final { scan-assembler-not "push"  { target { ! ia32 } } } } */
