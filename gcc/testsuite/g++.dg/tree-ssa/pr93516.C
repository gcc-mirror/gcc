// { dg-do compile }
// { dg-options "-O2" } */

struct b;
struct c {
  b *operator->();
};
class e {
  void *f;
  int d;

public:
  template <typename a> a g() { return *static_cast<a *>(this); }
};
struct h : e {};
struct b {
  void i(e);
  e j();
};
void m() {
  c k;
  h l = k->j().g<h>();
  k->i(l);
}
