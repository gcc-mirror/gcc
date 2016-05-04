// PR c++/70906
// { dg-do compile { target c++11 } }
// { dg-options "-Wall" }

template <typename> struct B;
template <typename U> struct F { typedef U *t; };
template <class> struct D {};
template <class VP> struct L {
  typedef VP np;
  typedef typename F<D<VP>>::t cnp;
};
struct P { typedef L<void *> nt; };
template <class N> struct I { typedef typename N::template A<int> t; };
template <class O1> struct Q { typedef typename I<O1>::t t; };
template <class T, class Hook, Hook T::*> struct G;
template <typename P, typename M, M P::*PM>
struct mh {
  template <class> struct A { typedef G<P, M, PM> pvt; };
};
template <typename T> struct B<T *> { static T pt(T); };
struct R : public D<void *> { typedef P ht; };
class lmh : public R {};
template <class T, class Hook, Hook T::*P> struct G {
  typedef Hook Ht;
  typedef typename Ht::ht::nt nt;
  typedef T vt;
  typedef typename nt::np np;
  typedef typename nt::cnp cnp;
  static np tnp(T &);
  static cnp tnp(const T &p1) {
    B<cnp>::pt(static_cast<const Ht &>(p1.*P));
    return cnp ();
  }
};
template <class T, class S> struct K {
  template <S> struct J;
  template <class U> static int foo(J<U::tnp> *, int);
  static const int c = sizeof(foo<T>(0, 0));
};
template <class V> struct W1 {
  typedef typename V::vt vt;
  static const bool value = K<V, typename V::np (*)(vt &)>::c == K<V, typename V::cnp (*)(const vt &)>::c;
};
template <class V> struct O {
  static const bool svt = W1<V>::value;
};
template <bool> struct M {};
template <class V> class C {
  static const bool svt = O<V>::svt;
  M<svt> m;
};
template <class V> struct H {
  C<V> bar();
};
template <class O1> struct ml {
  typedef typename Q<O1>::t po;
  typedef H<typename po::pvt> t;
};
template <class O1> class list : public ml<O1>::t {};
struct N {
  struct IV { lmh hk; };
  typedef list<mh<IV, lmh, &IV::hk>> ISL;
  friend void fn1(int &, N const &);
};
void fn1(int &, N const &) {
  N::ISL xl;
  for (xl.bar();;)
    ;
}
