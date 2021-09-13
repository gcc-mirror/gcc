/* { dg-do compile } */

typedef struct {
  short a;
  short b;
} c;
c *d;
int e, f, i, j, k, l, m, n, o, p;
c g, h;
void q() {
  do {
    if (o) {
      (*d).a = (*d).b = d[e].a = d[e].a * 3 + 1 >> 15;
      d[e].b = d[e].b * 3 + 1 >> 15;
    }
    n = -(d[e].b * g.b) >> 5;
    m = d[e].b * g.a + 1 >> 5;
    l = d[f].a * -d[f].b * h.b + 1 >> 5;
    k = d[f].a * h.b + d[f].b * h.a + 1 >> 5;
    j = n + l;
    i = m - k;
    (*d).a += j;
    d[e].a -= i;
    ++d;
  } while (--p);
}
