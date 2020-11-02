/* { dg-do compile } */

extern short i (void);

struct {
  short a;
  short b;
} c;

int d, e;
static int f = 1;

void g () {
  if (e) {
    if (f)
      goto L;
    while (d) {
      i ();
      short j = d, k = i (), l = k;
    L:
      if (!(d && e) || l)
        goto L;
      c.a = j;
      c.b = k;
    }
  }
}
