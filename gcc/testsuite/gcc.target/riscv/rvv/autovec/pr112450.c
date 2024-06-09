/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2 -mrvv-max-lmul=m8 -fno-vect-cost-model" } */

int a, b, d, e;
short c;
void f() {
  for (; e; e++) {
    int g = 6;
    for (; g > 2; g--) {
      int i = -8;
      while (i < 20) {
        i += 5;
        a += b;
      }
      c *= d;
    }
    b--;
  }
}
