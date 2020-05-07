// PR c++/82159
// { dg-do compile }
// { dg-options " -Wno-return-type" }

template <typename T> struct D { T e; };
struct F : D<int[0]> {
  F(const F &);
};
struct G : F {
  template <class T> G operator-(T);
};
template <class T> struct I {
  typedef typename T::template J<I> ak;
};
template <class T> struct K { typename I<T>::ak an; };
struct H {
  G l;
};
struct C {
  ~C();
};
template <class T> struct M : T {
  template <typename U, typename V> M(U, V);
  H h;
  virtual void foo() { T::bar(&h); }
};
template <int, typename> class A;
template <class> struct B {
  typedef int BT;
  struct BC {};
  template <class T> struct BD {
    G g;
    BD(BT, T n) : g(n.l - 0) {}
  };
  B(BT, BC);
};
template <typename> struct O;
template <int T, typename U>
struct O<B<A<T, U> > > : public B<A<T, U> >::BC {};
struct L : B<A<2, double> > {
  struct P : C {
    void bar(H *x) {
      BT a;
      BD<H>(a, *x);
    }
  };
  template <typename U, typename V> L(U x, V n) : B(x, n) {}
  int ll;
  virtual int baz() { M<P>(this, ll); }
};
template <typename> class Q {
  O<B<A<2, double> > > q;
  virtual L baz() { L(0, q); }
};
template <template <class> class T> struct R {
  R() { T<int>(); }
};
struct S {
  template <class> class J : R<Q> {};
};
void foo() { K<S> c; }

int main() {
  return 0;
}
