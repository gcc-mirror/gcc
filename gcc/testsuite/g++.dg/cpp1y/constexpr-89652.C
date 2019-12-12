// PR c++/89652
// { dg-do compile { target c++14 } }
// { dg-options "" }

template <typename T> constexpr auto foo (T &e) { return e.foo (); }
template <typename T> constexpr auto bar (T &e) { return foo (e); }
template <typename T, int N> struct A { typedef T a[N]; };
template <typename T, unsigned long N> struct B {
  typedef T *b;
  typename A<T, N>::a d;
  constexpr b foo () { return d; }
};
template <typename> struct C { long m; };
struct D { long n; };
template <typename, unsigned long> struct E {
  B<C<int>, 1>::b p;
  constexpr D operator* () { return {p->m}; }
  constexpr E operator++ (int) { auto a{*this}; ++p; return a; }
};
template <typename T, unsigned long N>
constexpr bool operator!= (E<T, N> a, E<T, N>) { return a.p; }
template <unsigned long N, typename T, unsigned long M>
constexpr auto baz (B<T, M> s, B<D, N>)
{
  B<D, M> t{};
  auto q{foo (t)};
  using u = E<T, M>;
  auto v = u{bar (s)};
  auto w = u{};
  while (v != w)
    *q++ = *v++;
  return t;
}
constexpr auto a = B<C<int>, 5>{};
auto b = B<D, 0>{};
auto c = baz (a, b);
