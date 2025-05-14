// PR c++/118104
// { dg-do compile { target c++11 } }

template<typename... Zs> struct Z { };

template <class... Ts> struct X {
  template <class... Us> using W = Z<void(Ts, Us)...>;
  template <class... Us> using Y = X<void(Ts, Us)...>;
};

template <class A, class... P>
using foo = X<int, int>::W<A, P...>;

template <class A, class... P>
using bar = X<int, int>::Y<A, P...>;

void
g ()
{
  foo<int, int> f;
  bar<int, int> b;
}
