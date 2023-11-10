/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3" } */

int a, b, c;
static int *d = &a;
int e(int f) { return f == 0 ?: f; }
int g() {
  a = 1;
  for (; a <= 8; a++) {
    b = e(*d);
    c = -b;
  }
}
