/* { dg-do compile } */
/* { dg-options "-O2 -fno-move-loop-stores" } */

int a, c, d, e, **f, *g, h;
static int i() {}
void j() {
  do {
    f = &g;
    c = 1 + (1 >> a);
    if (2 + c >= c - a)
      goto k;
    g = &c;
  } while (i());
  c = a + c + c;
k:
  e = **f;
  for (int b = 0; b < 3; b++)
    h = d = d ^ e;
}
