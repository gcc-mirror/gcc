// PR target/93187
// { dg-do compile { target c++11 } }
// { dg-options "-O2" }
// { dg-additional-options "-fstack-protector-strong" { target fstack_protector } }
// { dg-additional-options "-fpie" { target pie } }

struct A;
struct B;
struct C { int operator () (B, const B &); };
struct D { typedef int *d; };
struct E { C g; };
struct F { F (D::d); friend bool operator==(F &, const int &); };
template <typename T, typename> struct H {
  typedef D *I;
  E l;
  I foo ();
  T t;
  F bar (I, const T &);
  F baz (const T &);
};
template <typename T, typename U>
F
H<T, U>::bar (I n, const T &o)
{
  while (n)
    if (l.g (t, o))
      n = 0;
  return 0;
}
template <typename T, typename U>
F
H<T, U>::baz (const T &n)
{
  D *r = foo ();
  F p = bar (r, n);
  return p == 0 ? 0 : p;
}
template <typename, typename U> struct J {
  H<B, U> h;
  B &q;
  void baz () { h.baz (q); }
};
enum K { L };
template <typename, K = L> struct M;
template <int> struct G {
  using N = J<int, A>;
  N *operator->();
};
template <typename, K T> struct M : public G<T> {
  using N = J<int, A>;
  N *foo () { return n; }
  N *n;
  int o;
};
template <int N>
inline typename G<N>::N *
G<N>::operator-> ()
{
  N *n = static_cast<M<J<int, A>> *>(this)->foo ();
  return n;
}
struct B { bool qux (); };
struct O {
  struct P { M<int> p; };
  static thread_local P o;
  int baz () const;
};
thread_local O::P O::o;
B be;
int
O::baz () const
{
  do
    o.p->baz ();
  while (be.qux ());
  __builtin_unreachable ();
}
