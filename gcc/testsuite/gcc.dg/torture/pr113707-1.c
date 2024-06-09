/* { dg-do compile } */

int printf(const char *, ...);
struct a {
  int b;
} n;
int a, c, d, e, f = 1, g, h, j = 1, k, l, m, o;
int main() {
  struct a p;
  int i;
  p.b = 1;
  if (!j)
    goto q;
  p.b = i = 0;
  for (; i < 1; i++)
    if (k)
      while (m)
      r:
      q:
        if (p.b)
          g = 1;
  while (1) {
    i = 0;
    for (; i < 5; i++)
      ;
    if (l) {
      while (h)
        ;
      if (o) {
        d = 0;
        for (; d < 8; d++)
          ;
      }
    }
    for (; e; e--)
      while (a)
        p = n;
    if (c)
      goto r;
    printf("0");
    if (f)
      break;
  }
  return 0;
}
