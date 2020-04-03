/* { dg-do compile } */
/* { dg-options "-O1" } */

struct g;
struct h {
  g *operator->();
};
class i {
  void *a;
  int b;

public:
  template <typename f> f j() { return *static_cast<f *>(this); }
};
struct k : i {};
struct l : k {};
struct m {
  i n();
  i o(l, l, int);
};
struct g {
  void m_fn4(k);
};
h a;
i b;
i m::n() {
  l c, d, e = o(d, c, 0).j<l>();
  a->m_fn4(e);
  return b;
}
