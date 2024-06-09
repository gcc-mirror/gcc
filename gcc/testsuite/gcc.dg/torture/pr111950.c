/* { dg-do compile } */
/* { dg-additional-options "-ftree-vectorize -fno-vect-cost-model" } */

int a, b, d;
int c[4];
unsigned e;
void f() {
  char g;
  for (; d; d++) {
    g = 1;
    for (; g >= 0; g--) {
      e = b >= 2 || a >> b ?: a;
      c[g] = e;
    }
  }
}
