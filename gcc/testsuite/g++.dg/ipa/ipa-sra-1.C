/* { dg-do compile } */
/* { dg-options "-O2 -fipa-sra"  } */

void fn1(int *, int *, double *, int *, double *);
int a, c, d, e, f, g, k;
double *b;
double h, i;
void fn2(int *p1, int *p2, double *p3) {
  int l = 0, j, q, r;
  double m, n, o, p, s, t, u;
  --p3;
  for (; a;) {
    if (c) {
      ++*p2;
      goto L170;
    }
    m = n = b[c];
    p = t = m;
    for (; j; ++j) {
      u = 1.;
      if (k) {
        s = o;
        u = -1.;
      }
    }
    i = u * p;
  L60:
    p3[1] = s;
    for (; d;)
      goto L60;
    fn1(&f, &g, &h, &l, &p3[1]);
    o = p3[1];
  L100:
    o *= i;
    if (e)
      goto L100;
  L170:;
  }
  if (*p1)
    for (;;) {
      if (r)
        q = *p2;
      d = q - j;
      r = j;
    }
}
