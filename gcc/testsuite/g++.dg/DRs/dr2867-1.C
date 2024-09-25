// CWG2867 - Order of initialization for structured bindings.
// { dg-do run { target c++11 } }
// { dg-options "" }

#define assert(X) do { if (!(X)) __builtin_abort(); } while (0)

namespace std {
  template<typename T> struct tuple_size;
  template<int, typename> struct tuple_element;
}

int c, d, i;

struct A {
  A () { assert (c == 3); ++c; }
  ~A () { assert (c == 12); ++c; }
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

void
bar ()
{
  c = 1;
  const auto &[x, y, z, w] = foo (B {}, B {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (c == 11);
  ++c;
  d = 1;
  const auto &[s, t, u] = foo (C {}, C {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (d == 4);
}

template <int N>
void
baz ()
{
  c = 1;
  const auto &[x, y, z, w] = foo (B {}, B {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (c == 11);
  ++c;
  d = 1;
  const auto &[s, t, u] = foo (C {}, C {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (d == 4);
}

template <typename T, typename U>
void
qux ()
{
  c = 1;
  const auto &[x, y, z, w] = foo (T {}, T {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (c == 11);
  ++c;
  d = 1;
  const auto &[s, t, u] = foo (U {}, U {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (d == 4);
}

void
corge ()
{
  c = 1;
  auto [x, y, z, w] = foo (B {}, B {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (c == 11);
  ++c;
  d = 1;
  auto [s, t, u] = foo (C {}, C {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (d == 4);
}

template <int N>
void
garply ()
{
  c = 1;
  auto [x, y, z, w] = foo (B {}, B {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (c == 11);
  ++c;
  d = 1;
  auto [s, t, u] = foo (C {}, C {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (d == 4);
}

template <typename T, typename U>
void
freddy ()
{
  c = 1;
  auto [x, y, z, w] = foo (T {}, T {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (c == 11);
  ++c;
  d = 1;
  auto [s, t, u] = foo (U {}, U {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (d == 4);
}

int
main ()
{
  bar ();
  assert (c == 13);
  baz <0> ();
  assert (c == 13);
  qux <B, C> ();
  assert (c == 13);
  corge ();
  assert (c == 13);
  garply <42> ();
  assert (c == 13);
  freddy <B, C> ();
  assert (c == 13);
}
