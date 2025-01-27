// CWG2867 - Order of initialization for structured bindings.
// { dg-do run { target c++11 } }
// { dg-options "" }
// { dg-add-options tls }
// { dg-require-effective-target tls_runtime }

#define assert(X) do { if (!(X)) __builtin_abort(); } while (0)

namespace std {
  template<typename T> struct tuple_size;
  template<int, typename> struct tuple_element;
}

int a, c, d, i;

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

A
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

int
bar (int &x, int y)
{
  x = y;
  return y;
}

int
baz (int &x, int y)
{
  assert (x == y);
  return y;
}

struct E {
  ~E () { assert (a == 2); }
};

thread_local E e;
thread_local int c1 = bar (c, 1);
thread_local const auto &[x, y, z, w] = foo (B {}, B {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
thread_local int c2 = baz (c, 11);				// { dg-warning "structured binding declaration can be 'thread_local' only in" "" { target c++17_down } .-1 }
thread_local int d1 = bar (d, 1);
thread_local const auto &[s, t, u] = foo (C {}, C {});		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
thread_local int d2 = baz (d, 4);				// { dg-warning "structured binding declaration can be 'thread_local' only in" "" { target c++17_down } .-1 }
thread_local int c3 = bar (c, 1);
thread_local auto [x2, y2, z2, w2] = foo (B {}, B {});		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
thread_local int c4 = baz (c, 11);				// { dg-warning "structured binding declaration can be 'thread_local' only in" "" { target c++17_down } .-1 }
thread_local int d3 = bar (d, 1);
thread_local auto [s2, t2, u2] = foo (C {}, C {});		// { dg-warning "structured bindings only available with" "" { target c++14_down } }
thread_local int d4 = baz (d, 4);				// { dg-warning "structured binding declaration can be 'thread_local' only in" "" { target c++17_down } .-1 }

int
main ()
{
  volatile int u = c1 + x + y + z + w + c2;
  u += d1 + s + t + u + d2;
  u += c3 + x2 + y2 + z2 + w2 + c4;
  u += d3 + s2 + t2 + u2 + d4;
  assert (a == 0);
}
