// PR c++/83205
// { dg-do compile { target c++11 } }
// { dg-options "" }

struct A { int i; };
struct B { int i; };
namespace std {
  template <typename T> struct tuple_size;
  template <> struct tuple_size<A> {
    static constexpr int value = -1;
  };
#ifdef __SIZEOF_INT128__
  template <> struct tuple_size<B> {
    static constexpr unsigned __int128 value = -1;
  };
#endif
}

auto [a] = A{};	// { dg-error "1 name provided" }
		// { dg-message "while 'A' decomposes into -1 elements" "" { target *-*-* } .-1 }
		// { dg-warning "structured bindings only available with" "" { target c++14_down } .-2 }
#ifdef __SIZEOF_INT128__
auto [b] = B{};	// { dg-error "1 name provided" "" { target int128 } }
		// { dg-message "while 'B' decomposes into \[0-9xa-fXA-F]* elements" "" { target int128 } .-1 }
		// { dg-warning "structured bindings only available with" "" { target { c++14_down && int128 } } .-2 }
auto [c, d] = B{};	// { dg-error "2 names provided" "" { target int128 } }
			// { dg-message "while 'B' decomposes into \[0-9xa-fXA-F]* elements" "" { target int128 } .-1 }
			// { dg-warning "structured bindings only available with" "" { target { c++14_down && int128 } } .-2 }
#endif
auto [e, f, g] = A{};	// { dg-error "3 names provided" }
			// { dg-message "while 'A' decomposes into -1 elements" "" { target *-*-* } .-1 }
			// { dg-warning "structured bindings only available with" "" { target c++14_down } .-2 }
