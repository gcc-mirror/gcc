/* { dg-do compile } */
/* { dg-options "-O3 -fno-tree-forwprop -fno-tree-fre" } */
extern void o();
int a, b, c, d, e, f, g, h, i, k, l, m, n;
volatile int j;
static void p() {
  if (d) {
  q:
    if (a) {
      if (k) {
        if (!(d && e))
          goto r;
        if (i)
          goto q;
        o();
      }
      h || j;
    }
  s:
    d || j;
    if (a)
      goto q;
  r:
    if (b) {
      if (c)
        goto t;
      if (b)
        goto r;
      if (m)
        goto q;
    }
    while (j)
      ;
  u:
    if (g) {
      o();
      goto s;
    }
    if (h) {
    t:
      if (n)
        goto v;
      o();
      goto r;
    }
    int w = i & 1;
  v:
    if (w <= l)
      if (f)
        goto u;
    goto q;
  }
  if (a)
    goto v;
}
int main() { p(); }
