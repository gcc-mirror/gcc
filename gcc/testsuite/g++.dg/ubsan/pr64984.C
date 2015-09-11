// PR sanitizer/64984
// { dg-do compile }
// { dg-options "-fsanitize=vptr -std=gnu++11" }

template <typename X, X> struct K
{
  static constexpr X v = 0;
  typedef K t;
};
template <typename...> struct A;
template <typename X, typename Y>
struct A<X, Y> : Y
{
};
template <typename X> X M ();
template <typename...> struct B;
template <typename X, typename Y>
struct B<X, Y> : K<int, noexcept (static_cast<X>(M<Y>()))>
{
};
template <typename X, typename... Y>
struct G : A<int, B<X, Y...>>::t
{
};
template <typename X> struct J : G<X, X&&>
{
};
template <typename X> X&& foo (X&);
template <typename X> X&& bar (X&&);
template <typename X> struct P
{
  P (X& x) : q (x) {}
  X q;
};
template <typename...> struct Q;
template <typename X>
struct Q<X> : P<X>
{
  typedef P<X> r;
  X& s (Q&);
  Q (X& x) : r (x) {}
  Q (Q&& x) noexcept (J<X>::v) : r (foo<X>(s (x)))
  {
  }
};
template <typename... X> struct I : Q<X...>
{
  I ();
  I (X&... x) : Q<X...>(x...)
  {
  }
};
template <typename... X>
I<X&&...> baz (X&&... x)
{
  return I <X&&...> (foo<X>(x)...);
}
template <typename X> struct F
{
  int p;
  void operator[] (X&& x)
  {
    baz (bar (x));
  }
};
struct U
{
  virtual ~U ();
};

int
main ()
{
  F<U> m;
  m[U ()];
}
