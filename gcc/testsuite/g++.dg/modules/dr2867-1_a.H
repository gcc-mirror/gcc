// CWG2867 - Order of initialization for structured bindings.
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

#define assert(X) do { if (!(X)) __builtin_abort(); } while (0)

namespace std {
  template<typename T> struct tuple_size;
  template<int, typename> struct tuple_element;
}

extern int a, c, d, i;

struct A {
  A () { assert (c == 3); ++c; }
  ~A () { ++a; }
  template <int I> int &get () const { assert (c == 5 + I); ++c; return i; }
};

template <> struct std::tuple_size <A> { static const int value = 4; };
template <int I> struct std::tuple_element <I, A> { using type = int; };
template <> struct std::tuple_size <const A> { static const int value = 4; };
template <int I> struct std::tuple_element <I, const A> { using type = int; };

struct B {
  B () { assert (c >= 1 && c <= 2); ++c; }
  ~B () { assert (c >= 9 && c <= 10); ++c; }
};

struct C {
  constexpr C () {}
  constexpr C (const C &) {}
  template <int I> int &get () const { assert (d == 1 + I); ++d; return i; }
};

template <> struct std::tuple_size <C> { static const int value = 3; };
template <int I> struct std::tuple_element <I, C> { using type = int; };
template <> struct std::tuple_size <const C> { static const int value = 3; };
template <int I> struct std::tuple_element <I, const C> { using type = int; };

inline A
foo (const B &, const B &)
{
  A a;
  assert (c == 4);
  ++c;
  return a;
}

constexpr C
foo (const C &, const C &)
{
  return C {};
}

inline int
bar (int &x, int y)
{
  x = y;
  return y;
}

inline int
baz (int &x, int y)
{
  assert (x == y);
  return y;
}

struct E {
  ~E () { assert (a == 2); }
};

namespace {
E e;
int c1 = bar (c, 1);
const auto &[x, y, z, w] = foo (B {}, B {});
int c2 = baz (c, 11);
int d1 = bar (d, 1);
const auto &[s, t, u] = foo (C {}, C {});
int d2 = baz (d, 4);
int c3 = bar (c, 1);
auto [x2, y2, z2, w2] = foo (B {}, B {});
int c4 = baz (c, 11);
int d3 = bar (d, 1);
auto [s2, t2, u2] = foo (C {}, C {});
int d4 = baz (d, 4);
}
