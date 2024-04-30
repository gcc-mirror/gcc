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
  auto [ c [[]], d, e [[gnu::deprecated]] ] = a;	// { dg-warning "structured bindings with attributed identifiers only available with" "" { target { c++17 && c++23_down } } }
							// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
							// { dg-message "declared here" "" { target *-*-* } .-2 }
  ++c;
  ++d;
  ++e;							// { dg-warning "'e' is deprecated" }
  auto [ f, h [[gnu::deprecated]] [[]], i [[]], j ] = b;// { dg-warning "structured bindings with attributed identifiers only available with" "" { target { c++17 && c++23_down } } }
							// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
							// { dg-message "declared here" "" { target *-*-* } .-2 }
  ++f;
  ++h;							// { dg-warning "'h' is deprecated" }
  ++i;
  ++j;
  for (auto [ k, l [[gnu::deprecated]], m, n [[]]] : z)	// { dg-warning "structured bindings with attributed identifiers only available with" "" { target { c++17 && c++23_down } } }
    {							// { dg-warning "structured bindings only available with" "" { target c++14_down } .-1 }
							// { dg-message "declared here" "" { target *-*-* } .-2 }
      ++k;
      ++l;						// { dg-warning "'l' is deprecated" }
      ++m;
      ++n;
    }
}
