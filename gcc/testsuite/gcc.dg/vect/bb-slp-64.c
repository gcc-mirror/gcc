/* { dg-do compile } */

enum { a, b };
double *c, *e;
int d, f;
void g() {
  for (;;) {
    c[a] = c[b] = d * e[b];
    f = d -= f;
  }
}
