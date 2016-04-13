struct A;
template <class T>
struct Q { Q (T); };
template<typename T, class D>
struct U {
  ~U () { m1 (nullptr); }
  D m2 ();
  T *u;
  void m1 (T *) { m2 () (u); }
};
struct F { F (int *); };
template <class, class T = F>
using W = Q<T>;
int a, b;
void fn1 (void *);
template <class T>
void
fn2 (T *x)
{
  if (x)
    x->~T();
  fn1 (x);
}
template <typename T>
struct C {
  void operator() (T *x) { fn2 (x); }
};
struct D;
template <typename T, typename D = C<T> >
using V = U<T, D>;
struct A {
  A (int *);
};
struct S;
struct G {
  V<S> m3 ();
};
struct S {
  int e;
  virtual ~S () {}
};
template<typename T>
struct H {
  H (int, T x, int) : h(x) {}
  G g;
  void m4 () { g.m3 (); }
  T h;
};
struct I {
  I(A, W<D>);
};
void
test ()
{
  A c (&b);
  W<D> d (&b);
  I e (c, d);
  H<I> f (0, e, a);
  f.m4 ();
}

