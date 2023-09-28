/* { dg-do compile } */

int a, b, c, d, e;
static void f() {
  int *g = &b;
  b = 1;
  for (; b >= 0; b--) {
    c = 0;
    for (; c <= 1; c++)
      e = 0;
    for (; e <= 1; e++) {
      int h, i = h = 13;
      for (; h; h--)
        i = i << a;
      d &= i + c + 9 + *g;
    }
  }
}
int main() {
  f();
  for (;;)
    ;
}
