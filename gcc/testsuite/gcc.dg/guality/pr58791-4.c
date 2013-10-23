/* PR tree-optimization/58791 */
/* { dg-do run } */
/* { dg-options "-g -ffast-math" } */

#include "../nop.h"

__attribute__((noinline, noclone)) double
foo (float a, float b, float c, float d, float l, double u)
{
  float e = a * d;
  float f = d * e;
  double g = (double) f;
  double h = (double) b;
  double i = g * h;			/* { dg-final { gdb-test pr58791-4.c:32 "i" "486" { target { x86_64-*-* && lp64 } } } } */
  double i2 = i + 1.0;			/* { dg-final { gdb-test pr58791-4.c:32 "i2" "487" { target { x86_64-*-* && lp64 } } } } */
  double j = i * 3.25;
  double k = h + j;
  float m = l * 8.75;
  double n = (double) m;
  double o = (double) a;
  double p = n * o;
  double q = h * p;
  double r = q * 2.5;
  double s = k - r;
  double t = (double) c;
  double v = o * u;
  double w = o * v;
  double x = h * w;
  double y = h * x;
  double z = y * 8.5;
  asm volatile (NOP : : : "memory");
  asm volatile (NOP : : : "memory");
  return s - z;
}

int
main ()
{
  foo (3.0f, 2.0f, -1.0f, 9.0f, 1.0f, 2.0);
  return 0;
}
