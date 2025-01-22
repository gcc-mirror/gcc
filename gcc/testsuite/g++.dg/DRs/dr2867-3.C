// CWG2867 - Order of initialization for structured bindings.
// { dg-do run { target c++11 } }
// { dg-options "" }

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
foo (const int &, const int &)
{
  assert (false);
}

inline void
bar ()
{
  c = 1;
  static const auto &[x, y, z, w] = foo (B {}, B {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (c == 11);					// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
  ++c;
  d = 1;
  static const auto &[s, t, u] = foo (C {}, C {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (d == 4);					// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
}

template <int N>
inline void
baz ()
{
  c = 1;
  static const auto &[x, y, z, w] = foo (B {}, B {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (c == 11);					// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
  ++c;
  d = 1;
  static const auto &[s, t, u] = foo (C {}, C {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (d == 4);					// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
}

template <typename T, typename U>
inline void
qux ()
{
  c = 1;
  static const auto &[x, y, z, w] = foo (T {}, T {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (c == 11);					// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
  ++c;
  d = 1;
  static const auto &[s, t, u] = foo (U {}, U {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (d == 4);					// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
}

inline void
corge ()
{
  c = 1;
  static auto [x, y, z, w] = foo (B {}, B {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (c == 11);				// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
  ++c;
  d = 1;
  static auto [s, t, u] = foo (C {}, C {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (d == 4);				// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
}

template <int N>
inline void
garply ()
{
  c = 1;
  static auto [x, y, z, w] = foo (B {}, B {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (c == 11);				// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
  ++c;
  d = 1;
  static auto [s, t, u] = foo (C {}, C {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (d == 4);				// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
}

template <typename T, typename U>
inline void
freddy ()
{
  c = 1;
  static auto [x, y, z, w] = foo (T {}, T {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (c == 11);				// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
  ++c;
  d = 1;
  static auto [s, t, u] = foo (U {}, U {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (d == 4);				// { dg-warning "structured binding declaration can be 'static' only in" "" { target c++17_down } .-1 }
}

struct E {
  ~E () { assert (a == 6); }
};

int
main ()
{
  static E e;
  bar ();
  assert (c == 12);
  baz <0> ();
  assert (c == 12);
  qux <B, C> ();
  assert (c == 12);
  corge ();
  assert (c == 12);
  garply <42> ();
  assert (c == 12);
  freddy <B, C> ();
  assert (c == 12);
  assert (a == 0);
}
