// PR c++/53349
// { dg-do compile { target c++11 } }

template <int N>
struct Foo {
  constexpr Foo(const Foo<N-1> a) : m_a(a)     {}
  constexpr Foo(const Foo<N> &a)  : m_a(a.m_a) {}

  Foo<N-1> m_a;
};

template <> struct Foo<0> {};

constexpr Foo<1> catty1(Foo<1> x) { return x; }
constexpr Foo<2> catty2(Foo<1> x) { return Foo<2>(catty1(x)); }

constexpr auto res = catty2(Foo<1>(Foo<0>()));
