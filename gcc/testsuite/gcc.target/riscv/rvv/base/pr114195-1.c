/* Test that we do not have ice when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O3 -ftree-vectorize" } */

long a, b;
extern short c[];

void d() {
  for (int e = 0; e < 35; e = 2) {
    a = ({ a < 0 ? a : 0; });
    b = ({ b < 0 ? b : 0; });

    c[e] = 0;
  }
}
