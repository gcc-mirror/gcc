/* PR bootstrap/113445 */
/* { dg-do compile } */
/* { dg-options "-O2 -fcompare-debug -Wno-uninitialized" } */

enum E { F };
int *a, *b, *c, *d, e, f, g, h, i;
int *f1 (int *);
int *f2 (...);
int *f3 (int *, int);
int *f4 (unsigned, int);
int *f5 (enum E, int);
int *f6 (int, int);
enum E j;

void
f7 (void)
{
  int *k, *l, *m, *n, *o, *p, *q, *r, *s, *t, *u, *v, w, x, y, z, aa;
  v = f6 (0, 1);
  u = d;
  t = c;
  s = a;
  f4 (f, 1);
  f4 (f, 1);
  int *ab = f4 (f, 1);
  k = b;
  int ac = *f3 (&e, F);
  r = f1 (&ac);
  int ad = *f3 (&e, F);
  f1 (&ad);
  q = f5 (j, 0);
  p = f1 (t);
  f1 (o);
  f2 ();
  f2 ();
  f2 ();
  f2 ();
  f2 ();
  f2 ();
  f2 (z, z);
  f2 ();
  f2 ();
  f2 ();
  f1 (&h);
  f2 (z, k, ab, aa, r, g, (void *) 0);
  f2 (aa, w, &aa, r, k, (void *) 0);
  f2 (q, (void *) 0);
  f2 ();
  f2 (&aa, s, r, o, o, z, (void *) 0);
  n = f2 (s, (void *) 0);
  f2 (v, u, p, t, p, p, (void *) 0);
  m = f2 (v, u, x, y, x, x, (void *) 0);
  l = f2 (v);
  f2 (v, s);
  f2 (t, u);
  f2 (&aa, i, o, t, t, t, t);
  f2 (v, u, x, t, y, x, x, o, t, t, t, t, t);
  f2 (t);
  f2 (t, t, t, t, t, t, t, z, y);
  f2 (t, l, m, n);
}
