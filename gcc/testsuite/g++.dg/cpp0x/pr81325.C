// PR target/81325
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -fcompare-debug" }

struct A { A(const char *, const int & = 0); };
template <typename> struct B;
template <typename> struct C {
  int _M_i;
  void m_fn1() { __atomic_fetch_add(&_M_i, 0, __ATOMIC_RELAXED); }
};
struct D {
  int *Data;
  long Length = 0;
  D(int) : Data() {}
};
template <> struct B<int> : C<int> {};
struct F {
  B<int> RefCount;
  void m_fn2() { RefCount.m_fn1(); }
};
struct G {
  F *Obj;
  G(const G &p1) : Obj(p1.Obj) {
    if (Obj) {
      F *a = 0;
      a->m_fn2();
    }
  }
};
struct H {
  int CPlusPlus : 1;
};
struct I {
  enum {} KindId;
};
template <typename ResultT, typename ArgT> struct J {
  void operator()();
  ResultT operator()(ArgT) { return ResultT(); }
};
struct K {
  int AllowBind;
  I SupportedKind;
  I RestrictKind;
  G Implementation;
};
struct L {
  L(int) : Implementation(Implementation) {}
  K Implementation;
};
struct M {
  int Param1;
};
struct N {
  N(int, L &p2) : Param2(p2) {}
  L Param2;
};
struct O {
  L m_fn3();
};
L ignoringImpCasts(L);
J<O, L> b;
L hasName(const A &);
M hasOverloadedOperatorName(D);
J<O, int> c;
struct P {
  void m_fn4(L, int);
};
struct Q {
  void m_fn5(P *);
};
H d;
void Q::m_fn5(P *p1) {
  if (!d.CPlusPlus) {
    c();
    L e = 0, f = ignoringImpCasts(e);
    b(ignoringImpCasts(f)).m_fn3();
  }
  hasOverloadedOperatorName(0);
  hasName("");
  L g = 0;
  N(0, g);
  L h(0);
  p1->m_fn4(h, 0);
}
