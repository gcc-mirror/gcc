/* { dg-do compile } */
/* { dg-options "-O3 -std=gnu99 -ffast-math -fno-finite-math-only -march=z13" } */

int b, c, d;
double *e;
int f() {
  double *a = a;
  int g = d, f = c, h = b;
  if (__builtin_expect(f, 0))
    for (; g < h; g++)
      e[g] = (int)(a[g] >= 0.0 ? g + 0.99999999 : a[g]);
  return 0;
}
