/* { dg-do compile } */
/* { dg-additional-options "-march=bdver4" { target i?86-*-* x86_64-*-* } } */

typedef struct {
  struct {
    int a;
    int b;
  } c;
} * d;
typedef struct {
  unsigned e;
  d f[];
} g;
g h;
d *k;
int i(int j) {
  if (j) {
    *k = *h.f;
    return 1;
  }
  return 0;
}
int l;
int m;
int n;
d o;
void p() {
  for (; i(l); l++) {
    n += o->c.a;
    m += o->c.b;
  }
}
