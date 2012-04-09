/* { dg-do compile } */
/* { dg-options "-O" } */

int a, b, d, e, f, i, j, k, l, m;
unsigned c;
int g[] = { }, h[0];

int
fn1 () {
  return 0;
}

void
fn2 () {
  c = 0;
  e = 0;
  for (;; e = 0)
    if (f > j) {
      k = fn1 ();
      l = (d || k) * b;
      m = l * a;
      h[0] = m <= i;
    } else
      i = g[c];
}
