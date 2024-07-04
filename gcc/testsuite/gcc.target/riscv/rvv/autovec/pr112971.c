/* { dg-do compile }  */
/* { dg-options "-march=rv64gcv_zvl256b -mabi=lp64d -O3 -fno-vect-cost-model" }  */

int a;
short b[9];
char c, d;
void e() {
  d = 0;
  for (;; d++) {
    if (b[d])
      break;
    a = 8;
    for (; a >= 0; a--) {
      char *f = &c;
      *f &= d == (a & d);
    }
  }
}
