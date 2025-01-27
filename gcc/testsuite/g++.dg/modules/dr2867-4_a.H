// CWG2867 - Order of initialization for structured bindings.
// { dg-require-effective-target c++20 }
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }
// { dg-add-options tls }
// { dg-require-effective-target tls_runtime }

#define assert(X) do { if (!(X)) __builtin_abort(); } while (0)

namespace std {
  template<typename T> struct tuple_size;
  template<int, typename> struct tuple_element;
}

extern int a, c;

struct C {
  C () { assert (c >= 5 && c <= 17 && (c - 5) % 4 == 0); ++c; }
  ~C () { assert (c >= 8 && c <= 20 && c % 4 == 0); ++c; }
};

struct D {
  D () { assert (c >= 7 && c <= 19 && (c - 7) % 4 == 0); ++c; }
  ~D () { assert (a % 5 != 4); ++a; }
};

struct A {
  A () { assert (c == 3); ++c; }
  ~A () { assert (a % 5 == 4); ++a; }
  template <int I> D get (const C & = C{}) const { assert (c == 6 + 4 * I); ++c; return D {}; }
};

template <> struct std::tuple_size <A> { static const int value = 4; };
template <int I> struct std::tuple_element <I, A> { using type = D; };
template <> struct std::tuple_size <const A> { static const int value = 4; };
template <int I> struct std::tuple_element <I, const A> { using type = D; };

struct B {
  B () { assert (c >= 1 && c <= 2); ++c; }
  ~B () { assert (c >= 21 && c <= 22); ++c; }
};

inline A
foo (const B &, const B &)
{
  A a;
  assert (c == 4);
  ++c;
  return a;
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
  ~E () { assert (a == 5); }
};

namespace {
thread_local E e;
thread_local int c1 = bar (c, 1);
// First B::B () is invoked twice, then foo called, which invokes A::A ().
// e is reference bound to the A::A () constructed temporary.
// Then 4 times (in increasing I):
//   C::C () is invoked, get is called, D::D () is invoked, C::~C () is
//   invoked.
// After that B::~B () is invoked twice.
// At exit time D::~D () is invoked 4 times, then A::~A ().
thread_local const auto &[x, y, z, w] = foo (B {}, B {});
thread_local int c2 = baz (c, 23);
}
