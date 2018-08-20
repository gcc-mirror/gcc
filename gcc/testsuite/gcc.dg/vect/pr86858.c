/* { dg-do compile } */

int a, b, c, d;
char e(char f, char g) { return f + g; }
void h() {
  for (; c; ++c) {
    d = 0;
    for (; d != 8; d = e(d, 3)) {
      a = b && a;
      b = c;
    }
  }
}
