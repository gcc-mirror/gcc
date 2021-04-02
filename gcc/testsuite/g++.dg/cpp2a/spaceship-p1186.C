// PR c++/96299
// P1186R3
// { dg-do compile { target c++20 } }

#include <compare>

struct H {
  bool operator==(H const &) const;
  bool operator<(H const &) const;
};

// Can't deduce category from op< and op==.
struct A {
  int i;
  char c;
  H h;				// { dg-error "operator<=>" }
  auto operator<=>(A const &) const = default;
};

// OK, explicit return type tells us how to interpret op</==.
struct B {
  int i;
  char c;
  H h;
  std::strong_ordering operator<=>(B const &) const = default;
};

struct C {
  bool operator<(C const &) const;
};

// C's op< isn't enough, need op== as well.
struct D {
  C c;				// { dg-error "operator<=>" }
  std::strong_ordering operator<=>(D const &) const = default;
};

struct E {
  std::partial_ordering operator<=>(E const &) const;
};

// Can't use a weak op<=> to build a strong op<=>.
struct F {
  E e;				// { dg-error "strong_ordering" }
  H h;
  std::strong_ordering operator<=>(F const &) const = default;
};

struct G {
  std::strong_ordering operator<=>(G const &) const;
};

// OK, uses op<=> for g and op</op== for h.
struct I {
  G g;
  H h;
  std::partial_ordering operator<=>(I const &) const = default;
};

template <class T>
struct J {
  T t;				// { dg-error "no match|ambiguous" }
  auto operator<=>(J const &) const = default;
};

template <class T>
struct K {
  T t;				// { dg-error "no match" }
  std::partial_ordering operator<=>(K const &) const = default;
};

template <class T>
struct M {
  T t;				// { dg-error "no match|strong_ordering" }
  std::strong_ordering operator<=>(M const &) const = default;
};

// Test that we don't fall back to </== if <=> is ambiguous.
struct N {
  bool operator==(N const &) const;
  bool operator<(N const &) const;
};
template <class T>
std::partial_ordering operator<=>(N const &, T&&);
template <class T>
std::partial_ordering operator<=>(T&&, N const &);

void
foo (A &a1, A &a2, B &b1, B &b2, D& d1, D& d2, F& f1, F& f2, I& i1, I& i2)
{
  auto a = a1 < a2;		// { dg-error "deleted" }
  auto b = b1 < b2;
  auto d = d1 < d2;		// { dg-error "deleted" }
  auto f = f1 < f2;		// { dg-error "deleted" }
  auto i = i1 < i2;

  auto j1 = J<int>() < J<int>();
  auto j2 = J<H>() < J<H>();	// { dg-error "deleted" }
  auto j3 = J<C>() < J<C>();	// { dg-error "deleted" }
  auto j4 = J<E>() < J<E>();
  auto j5 = J<G>() < J<G>();
  auto j6 = J<N>() < J<N>();	// { dg-error "deleted" }

  auto k1 = K<int>() < K<int>();
  auto k2 = K<H>() < K<H>();
  auto k3 = K<C>() < K<C>();	// { dg-error "deleted" }
  auto k4 = K<E>() < K<E>();
  auto k5 = K<G>() < K<G>();
  auto k6 = K<N>() < K<N>();	// { dg-error "deleted" }

  auto m1 = M<int>() < M<int>();
  auto m2 = M<H>() < M<H>();
  auto m3 = M<C>() < M<C>();	// { dg-error "deleted" }
  auto m4 = M<E>() < M<E>();	// { dg-error "deleted" }
  auto m5 = M<G>() < M<G>();
  auto m6 = M<N>() < M<N>();	// { dg-error "deleted" }
}
