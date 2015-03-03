/* { dg-do compile } */
/* { dg-options "-O3 -c -w" } */

template <class> class A;
template <class R> struct VirtualMatrice {
  virtual bool m_fn1(int) const { return true; }
  struct B {
    A<R> x;
    B(VirtualMatrice *p1, A<R> p2) : x(p2) { p1->m_fn1(0) ?: throw; }
  };
  void operator*(A<R> p1) { B(this, p1); }
  ~VirtualMatrice();
}
;
template <class> class A {
public:
  operator int *();
  A(int *, long);
};

class G : public A<int> {
public:
  G(long);
};
int typedef Complex;
template <class> class H : VirtualMatrice<int> {};
template <class> class C;
template <> class C<int> : H<Complex>, VirtualMatrice<Complex> {
  bool m_fn1(int) const { return true; }
};
template <class K, class Mat>
void DoIdoAction(int, int, A<K> p3, A<K>, A<K>, A<K>, Mat, Mat &p8) {
  p8 *p3;
}

class D {
  typedef int K;
  class F {
    int operator()() const;
  };
};
int D::F::operator()() const {
  VirtualMatrice<K> *a;
  VirtualMatrice<K> b, &B = *a;
  G c(0), g(1);
  int d, e, f;
  A<K> h(&g[f], 0), i(&g[e], 0), j(&g[d], 0);
  DoIdoAction(0, 3, h, i, j, c, b, B);
}
