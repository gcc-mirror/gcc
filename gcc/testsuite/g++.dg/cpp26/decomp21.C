// P1061R10 - Structured Bindings can introduce a Pack
// { dg-do run { target c++11 } }
// { dg-options "" }

using size_t = decltype (sizeof 0);

auto g () -> int (&)[4]
{
  static int a[4] = { 1, 2, 3, 4 };
  return a;
}

template <size_t N>
void
h (int (&arr)[N])
{
  auto [a, ...b, c] = arr;				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
							// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (b) == 2, "");
  auto &[f, ...g, h] = arr;				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
							// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (g) == 2, "");
  if (&f != &arr[0] || &h != &arr[3]
      || &g...[0] != &arr[1] || &g...[1] != &arr[2])	// { dg-warning "pack indexing only available with" "" { target c++23_down } }
    __builtin_abort ();
  auto &[...e] = arr;					// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
							// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof ... (e) == 4, "");
  if (&e...[0] != &arr[0] || &e...[3] != &arr[3])	// { dg-warning "pack indexing only available with" "" { target c++23_down } }
    __builtin_abort ();
}

struct C { int x, y, z; };

template <class T>
void
now_i_know_my ()
{
  auto [a, b, c] = C ();				// { dg-warning "structured bindings only available with" "" { target c++14_down } }
  auto [d, ...e] = C ();				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
							// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (e) == 2, "");
  auto [...f, g] = C ();				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
							// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (f) == 2, "");
  auto [h, i, j, ...k] = C ();				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
							// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (k) == 0, "");
//  auto [l, m, n, o, ...p] = C ();
}

auto foo () -> int (&)[2]
{
  static int a[2] = { 1, 2 };
  return a;
}

template <class T>
void
bar ()
{
  auto [...a] = foo ();					// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
							// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (a) == 2, "");
  auto [b, c, ...d] = foo ();				// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
							// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  static_assert (sizeof... (d) == 0, "");
}

struct D { };

void
baz (...)
{
  __builtin_abort ();
}

template <typename T>
void
qux ()
{
  D arr[1] = {};
  auto [...e] = arr;					// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
							// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  baz (e...);
}

int d;

void
baz (D)
{
  d = 1;
}

int
main ()
{
  h (g ());
  now_i_know_my <int> ();
  bar <int> ();
  qux <int> ();
}
