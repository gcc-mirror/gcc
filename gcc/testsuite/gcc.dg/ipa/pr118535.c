/* { dg-do run } */
/* { dg-options "-O2" } */

int a, b, c, d, e, f, *g = &b;
static int h(int i) { return i < 0 || i > a ? 0 : i << a; }
static int j(unsigned short i) {
  f = d == e;
  *g = h(65535 ^ i);
  return c;
}
int main() {
  j(0);
  h(0);
  if (b != 0)
    __builtin_abort();
  return 0;
}
