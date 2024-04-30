// { dg-do compile { target c++11 } }
// { dg-options "" }

namespace std {
  template<typename T> struct tuple_size;
  template<int, typename> struct tuple_element;
}

struct A {
  int i;
  template <int I> int& get() { return i; }
};

template<> struct std::tuple_size<A> { static const int value = 3; };
template<int I> struct std::tuple_element<I,A> { using type = int; };

struct B {
  int i, j;
  long long k, l;
} z[6];

void
foo (A &a, B &b)
{
  auto [ c [[]], d, e [[maybe_unused]] ] = a;		// { dg-warning "structured bindings with attributed identifiers only available with" "" { target { c++17 && c++23_down } } }
							// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  auto [ f, h [[maybe_unused]] [[]], i [[]], j ] = b;	// { dg-warning "structured bindings with attributed identifiers only available with" "" { target { c++17 && c++23_down } } }
							// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  for (auto [ k, l [[maybe_unused]], m, n [[]]] : z)	// { dg-warning "structured bindings with attributed identifiers only available with" "" { target { c++17 && c++23_down } } }
    ;							// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
  auto &[o[[]][[]][[]], p[[]], q[[]]] = a;		// { dg-warning "structured bindings with attributed identifiers only available with" "" { target { c++17 && c++23_down } } }
    ;							// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
}
