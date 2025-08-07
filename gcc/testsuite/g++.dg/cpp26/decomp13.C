// P1061R10 - Structured Bindings can introduce a Pack
// { dg-do compile { target c++11 } }
// { dg-options "" }

namespace std {
  template<typename T> struct tuple_size;
  template<int, typename> struct tuple_element;
}

struct S { int a, b, c, d; };
struct T {
  int a[5];
  template <int I> int &get () { return a[I]; }
};

template<> struct std::tuple_size<T> { static const int value = 5; };
template<int I> struct std::tuple_element<I,T> { using type = int; };

template <int N>
void
foo ()
{
  auto [a, ...b, c] = S ();		// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  auto [...d] = S ();			// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  auto [...e, f, ...g, h] = S ();	// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-error "multiple packs in structured binding declaration" "" { target *-*-* } .-2 }
  auto [i, j, k, l, ...m, n] = S ();	// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-error "6 names provided for structured binding" "" { target *-*-* } .-2 }
					// { dg-message "while 'S' decomposes into 4 elements" "" { target *-*-* } .-3 }
  auto [o, ...p, q, r, s] = S ();	// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  auto [t, u, v, w, x, ...y, z] = T ();	// { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-error "7 names provided for structured binding" "" { target *-*-* } .-2 }
					// { dg-message "while 'T' decomposes into 5 elements" "" { target *-*-* } .-3 }
  int aa[] = { 1, 2, 3 };
  const auto & [ab, ...ac, ad, ae, af] = aa; // { dg-warning "structured binding packs only available with" "" { target { c++17 && c++23_down } } }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
					// { dg-error "5 names provided for structured binding" "" { target *-*-* } .-2 }
					// { dg-message "while 'const int \\\[3\\\]' decomposes into 3 elements" "" { target *-*-* } .-3 }
}

void
bar ()
{
  auto [a, ...b, c, d] = S ();		// { dg-error "structured binding pack outside of template" }
					// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
}
