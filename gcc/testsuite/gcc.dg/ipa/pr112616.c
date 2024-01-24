/* { dg-do run } */
/* { dg-options "-O2" } */

unsigned a;
int b, d, e, f = 2, g, h = 1, *i = &b;
volatile int c = 1;
static int *o() {
  long m = ~a;
  int j = f / b, k = f - 1, n = m << -1 / ~g / k;
  if (j && n)
    c;
  return &e;
}
static long p() {
  int *q = 0, **r = &q;
  if (c) {
    *i = h;
    *r = o();
  }
  return *q;
}
int main() {
  p();
  int *l = 0;
  if (d)
    c = *l;
  return 0;
}
