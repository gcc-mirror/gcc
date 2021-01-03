/* { dg-do compile } */

typedef struct {
  double a, b;
} c;
int d, e;
int i(void);
void h(c, c);
void f() {
  c a, g;
  do {
    a.a = e ?: g.a;
    a.b = g.b + d;
    h(g, a);
    g = a;
  } while (i());
}
