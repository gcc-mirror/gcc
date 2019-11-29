/* { dg-do compile } */
/* { dg-additional-options "-O3" } */

int a, c;
int *b;
long d;
double *e;

void fn1() {
  long f;
  double g, h;
  while (c) {
    if (d) {
      g = *e;
      *(b + 4) = g;
    }
    if (f) {
      h = *(e + 2);
      *(b + 6) = h;
    }
    e += a;
    b += 8;
    c--;
    d += 2;
  }
}
