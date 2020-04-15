// { dg-do compile { target c++17 } }
// { dg-options "-O2 -fno-exceptions -Wno-return-type" }

template <class a> struct b { typedef a *c; };
class e {};
template <typename a> class d {
public:
  typedef typename b<a>::c c;
  c begin();
  c end();
};
struct f {
  enum { g } h;
};
struct i {
  d<f *> j();
};
struct l {
  d<i *> k();
};
class ac;
class o {
public:
  o(int *, int *, int *, ac *);
};
class ac {
public:
  ac(e);
  virtual o *ae(int *, int *, int *, int *);
};
class p {
  void af(f *m) {
    switch (m->h)
    case f::g:
      ag();
  }

public:
  void n() {
    l ah;
    for (i *ai : ah.k())
      for (f *m : ai->j())
        af(m);
  }
  virtual void ag() { __builtin_unreachable(); }
};
template <typename = int> class an : o {
public:
  an(int *, int *, int *, int *, ac *);
};
class q : ac {
public:
  q() : ac([]() -> e {}()) {}
  o *ae(int *ap, int *aq, int *ar, int *as) { an(ap, aq, ar, as, this); }
};
template <typename at>
an<at>::an(int *, int *aq, int *ar, int *as, ac *au) : o(aq, ar, as, au) {
  p().n();
}
void av() { new q; }
