/* { dg-do compile } */

int a, b, c, d, e;
int f() {
  c = 0;
  for (; c >= 0; c--) {
    d = 0;
    for (; d <= 0; d++) {
      e = 0;
      for (; d + c + e >= 0; e--)
        ;
      a = 1;
      b = 0;
      for (; a; ++b)
        a *= 2;
      for (; b + d >= 0;)
        return 0;
    }
  }
}
