/* { dg-do run } */
/* { dg-require-effective-target lp64 } */

int c, d, h, i, j, l, *n = &h;
short e, f, g, *k, m;
long o;
short p(short p1, int q) { return q >= 32 || p1 > 5 >> q ? 1 : p1 << q; }
long u(unsigned p1)
{
  int r = 50, s, *t = &c;
 L:
  m && (*k = 0);
  for (d = 1; d; d--)
    for (s = 0; s < 3; s++) {
      *n = i ^ p1;
      *t = p1 > (unsigned)p((unsigned)(o = 4073709551615) >= p1 && 5, r);
      if (f)
        goto L;
    }
  for (; e < 1;)
    return j;
  int *b[2] = {&s, &r};
  for (; l; l--) {
    long a[1];
    for (r = 0; r < 1; r++) {
      h = a[0];
      if (g)
        goto L;
    }
  }
  return 0;
}
int main()
{
  u(6);
  if (c != 1)
    __builtin_abort();
  return 0;
}
