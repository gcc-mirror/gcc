/* { dg-options "-fno-lto" } */

double __attribute__((noinline,noclone))
pow (double x, double y) { return 0.0; }
