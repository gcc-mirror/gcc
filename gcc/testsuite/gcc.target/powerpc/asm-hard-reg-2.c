/* { dg-do compile } */

double a;
double b (long double c) {
  long double d = 0;
  double e;
  __asm__ ("" : "=f" (a), "={fr2}" (e) : "{fr1}" (d));
  return c + c;
}
