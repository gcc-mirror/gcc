/* { dg-do compile } */
/* { dg-options "-O3 -fchecking" } */

/* PR tree-optimization/118344 */

/* This used to ICE attempting to extend a mask variable after testing the
   wrong mask variable.  */

int d, e, g, h, i, c, j;
static short k;
char o;
static int *p;
static long *a;
int b[0];
int q(int s, int t, int *u, int *v) {
  for (int f = 0; f < s; f++)
    if ((t & v[f]) != u[f])
      return 0;
  return 1;
}
int w(int s, int t) {
  int l[] = {t, t, t, t}, m[] = {e, e, 3, 1};
  int n = q(s, d, l, m);
  return n;
}
int x(unsigned s) {
  unsigned r;
  if (s >= -1)
    return 1;
  r = 10000000;
  while (s > 1 / r)
    r /= 2;
  return g ? 2 : 0;
}
void y() {
  for (;;) {
    b[w(8, *p)] = h;
    for (; a + k; j = o)
      i &= c = x(6) < 0;
  }
}
