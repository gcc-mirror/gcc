// CWG2867 - Order of initialization for structured bindings.
// { dg-do run { target c++11 } }
// { dg-options "" }

#define assert(X) do { if (!(X)) __builtin_abort(); } while (0)

namespace std {
  template<typename T> struct tuple_size;
  template<int, typename> struct tuple_element;
}

int c;

struct C {
  C () { assert (c >= 5 && c <= 17 && (c - 5) % 4 == 0); ++c; }
  ~C () { assert (c >= 8 && c <= 20 && c % 4 == 0); ++c; }
};

struct D {
  D () { assert (c >= 7 && c <= 19 && (c - 7) % 4 == 0); ++c; }
  ~D () { assert (c >= 24 && c <= 27); ++c; }
};

struct A {
  A () { assert (c == 3); ++c; }
  ~A () { assert (c == 28); ++c; }
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

A
foo (const B &, const B &)
{
  A a;
  assert (c == 4);
  ++c;
  return a;
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
  // First B::B () is invoked twice, then foo called, which invokes A::A ().
  // e is reference bound to the A::A () constructed temporary.
  // Then 4 times (in increasing I):
  //   C::C () is invoked, get is called, D::D () is invoked, C::~C () is
  //   invoked.
  // After that B::~B () is invoked twice, then the following 2 user
  // statements.
  // Then D::~D () is invoked 4 times, then A::~A ().
  const auto &[x, y, z, w] = foo (B {}, B {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (c == 23);
  ++c;
}

template <int N>
void
baz ()
{
  c = 1;
  const auto &[x, y, z, w] = foo (B {}, B {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (c == 23);
  ++c;
}

template <typename T>
void
qux ()
{
  c = 1;
  const auto &[x, y, z, w] = foo (T {}, T {});	// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  assert (c == 23);
  ++c;
}

int
main ()
{
  bar ();
  assert (c == 29);
  baz <42> ();
  assert (c == 29);
  qux <B> ();
  assert (c == 29);
}
