// PR tree-optimization/92262
// { dg-do compile { target c++11 } }
// { dg-options "-O2 -ftree-loop-distribution -g" }

struct A;
struct B { template <typename T> using b = T *; };
template <typename, typename T> using c = B::b<T>;
void *operator new (__SIZE_TYPE__, void *p) { return p; }
struct C {
  template <typename T, typename... U> void
  foo (T *x, U... y) { new (x) T(y...); }
};
template <typename> class D : public C {};
template <typename> struct E;
template <typename T> struct E<D<T>> {
  using e = D<T>;
  template <typename U> using f = D<U>;
  template <typename U, typename... V>
  static void
  bar (e x, U y, V... z) { x.foo (y, z...); }
};
template <typename T> struct F : E<T> {
  template <typename U> struct G { typedef typename E<T>::template f<U> O; };
};
template <typename T, typename U, typename V> void
baz (T x, U y, V z)
{
  F<V>::bar (z, y, *x);
}
struct H {
  typedef c<int, A> I;
  typedef c<int, I> J;
  I i;
  J j;
  void qux (J x) { j = x; }
};
template <typename>
struct K {
  K(D<A> x) : k (x) {}
  typedef H::J L;
  struct M { L m; H n, o; };
  struct N : F<D<int>>::G<A>::O, M { N (F<D>::G<A>::O); };
  void quux ();
  N k;
};
template <typename T>
void
K<T>::quux ()
{
  L K (k.m - 1);
  k.n.qux (K);
}
template <typename, typename = int>
struct P : K<int> {
  template <typename T>
  P (T x, T, D<A> y = D<A> ()) : K (y) { corge (x); }
  template <typename T> void corge (T);
  typedef L L;
};
template <typename T, typename U>
template <typename V>
void P<T, U>::corge (V y)
{
  quux ();
  for (L x = k.n.j; x < k.o.j; ++x)
  {
    ++y;
    D<int> pv;
    baz (y, *x, pv);
  }
  D<int> z;
  baz (y, k.o.i, z);
}
struct A {
  A (int x) : a (x) {}
  int a;
};
int a[2]{};

int
main ()
{
  P<int> (a, a);
  return 0;
}
