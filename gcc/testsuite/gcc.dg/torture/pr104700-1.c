/* { dg-do compile } */
/* { dg-additional-options "-ftree-pre" } */

int printf(const char *, ...);
int a, b, c = 2, d, e, *f, g;
void o() {
  unsigned h = 1;
  int j = -1, k, l = 1, m = 2, i;
  while (c < 2)
    ;
L1:
  k = h;
  h = -1;
  if (k < 2 && !c) {
    printf("%d", k);
    goto L1;
  }
  if (!j)
    l = printf("0");
  if (g)
    k = 0;
  if (a && k)
    goto L2;
  while (f) {
    m = a;
    d = i;
    i = e;
    f = &j;
  L2:
    if (d == l && !m)
      l = b;
  }
  unsigned *n[1] = {&h};
}
int main() {
  o();
  return 0;
}
