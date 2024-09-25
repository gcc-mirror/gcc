/* { dg-additional-options "-fcompare-debug" } */

/* PR rtl-optimization/116179 */

struct g *b;
struct g {};
void *operator new(__SIZE_TYPE__, void *);
enum c {};
struct d : g{} * e;
c f;
struct h {
  g *k() {
    d *a;
    c i;
    if (a || i == 0 || i == 1)
      if (e || f)
        return 0;
    return new (&j) d;
  }
  void n();
  g j;
};
void h::n() {
  for (g *l(b), *m(b); l; l = m, m = 0)
    k();
}
