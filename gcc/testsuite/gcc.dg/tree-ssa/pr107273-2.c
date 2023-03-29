/* { dg-do run } */
/* { dg-options "-Os" } */

int a, d, f;
char b, g;
unsigned i;
int main() {
  int c = 300, h = 40;
  char e = 1;
  for (; a < 1; a++) {
    c = ~((i - ~c) | e);
  L1:
    e = f = c;
    if (c)
      if (c > -200)
        e = g % (1 << h);
    char k = 0;
  L2:;
  }
  if (b) {
    if (d)
      goto L2;
    if (!b)
      goto L1;
  }
  return 0;
}
