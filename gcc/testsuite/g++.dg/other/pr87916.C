// PR middle-end/87916
// Testcase by Martin Liška <marxin@gcc.gnu.org>

// { dg-do compile }
// { dg-options "-O2 -g" }
// { dg-additional-options "-fPIC" { target fpic } }

struct a {
  virtual ~a();
};
template <typename b> class c {
public:
  class d {
  public:
    d(c);
    b *operator->();
  };
};
int e, f;
class g {
public:
  class h {
  public:
    virtual void j(g &, int &, bool) = 0;
  };
  c<h> k();
  int *l();
  int *m();
};
int *g::l() try {
  for (c<h>::d i(k());;)
    i->j(*this, e, true);
} catch (int) {
  return 0;
}
int *g::m() try {
  for (c<h>::d i(k());;)
    i->j(*this, f, false);
} catch (int) {
  return 0;
}
struct n : a, g::h {
  void o();
  void j(g &, int &, bool) { o(); }
};
