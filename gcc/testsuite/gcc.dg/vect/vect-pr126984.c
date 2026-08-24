/* { dg-do compile } */
/* { dg-additional-options "-ffast-math" } */
/* { dg-additional-options "-march=armv9-a" { target { aarch64-*-* } } } */

double *a;
double b, c, d, e, f;
int g, h;
void l() {
  double i, j, k;
  for (; g; g++, h += 2) {
    k = a[h];
    j = a[h + 1];
    a[h] = b * f - c * e + d * k - i * j;
    a[h + 1] = b * e + c * f + d * j + i * k;
  }
}
