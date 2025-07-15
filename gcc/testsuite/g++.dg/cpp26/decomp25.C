// C++26 P2686R4 - constexpr structured bindings
// { dg-do compile { target c++11 } }
// { dg-options "-fno-implicit-constexpr" }

namespace std {
  template <typename T> struct tuple_size;
  template <int, typename> struct tuple_element;
}

struct A {
  int i, j;
  template <int I> int &get () { return I == 1 ? j : i; }
};

template <> struct std::tuple_size <A> { static const int value = 3; };
template <int I> struct std::tuple_element <I, A> { using type = int; };
template <> struct std::tuple_size <const A> { static const int value = 3; };
template <int I> struct std::tuple_element <I, const A> { using type = int; };

struct B {
  int i, j;
  long long k, l;
} a[3] = { { 1, 2, 3, 4 }, { 5, 6, 7, 8 }, { 9, 10, 11, 12 } };	// { dg-message "'a' was not declared 'constexpr'" "" }

struct C {
  int i, j;
  template <int I> const int &get () const { return I == 1 ? j : i; }
};

template <> struct std::tuple_size <const C> { static const int value = 3; };
template <int I> struct std::tuple_element <I, const C> { using type = const int; };

constexpr auto [ b, c, d ] = a;				// { dg-warning "structured bindings only available with" "" { target c++14_down } }
							// { dg-warning "structured binding declaration can be 'constexpr' only with" "" { target c++23_down } .-1 }
							// { dg-error "the value of 'a' is not usable in a constant expression" "" { target *-*-* } .-2 }
#if __cpp_constinit >= 201907
constinit auto [ e, f, g ] = a;				// { dg-warning "'constinit' can be applied to structured binding only with" "" { target { c++20 && c++23_down } } }
							// { dg-error "'constinit' variable '<structured bindings>' does not have a constant initializer" "" { target c++20 } .-1 }
							// { dg-error "the value of 'a' is not usable in a constant expression" "" { target c++20 } .-2 }
#endif
constexpr auto [ h, i, j, k ] = a[1];			// { dg-warning "structured bindings only available with" "" { target c++14_down } }
							// { dg-warning "structured binding declaration can be 'constexpr' only with" "" { target c++23_down } .-1 }
							// { dg-error "the value of 'a' is not usable in a constant expression" "" { target *-*-* } .-2 }
#if __cpp_constinit >= 201907
constinit auto [ l, m, n, o ] = a[2];			// { dg-warning "'constinit' can be applied to structured binding only with" "" { target { c++20 && c++23_down } } }
							// { dg-error "'constinit' variable '<structured bindings>' does not have a constant initializer" "" { target c++20 } .-1 }
							// { dg-error "the value of 'a' is not usable in a constant expression" "" { target c++20 } .-2 }
#endif
constexpr auto & [ p, q, r ] = a;			// { dg-warning "structured bindings only available with" "" { target c++14_down } }
							// { dg-warning "structured binding declaration can be 'constexpr' only with" "" { target c++23_down } .-1 }
#if __cpp_constinit >= 201907
constinit auto & [ s, t, u ] = a;			// { dg-warning "'constinit' can be applied to structured binding only with" "" { target { c++20 && c++23_down } } }
#endif
constexpr auto & [ v, w, x, y ] = a[1];			// { dg-warning "structured bindings only available with" "" { target c++14_down } }
							// { dg-warning "structured binding declaration can be 'constexpr' only with" "" { target c++23_down } .-1 }
#if __cpp_constinit >= 201907
constinit auto & [ aa, ab, ac, ad ] = a[2];		// { dg-warning "'constinit' can be applied to structured binding only with" "" { target { c++20 && c++23_down } } }
#endif

A z = { 42, -42 };					// { dg-message "'z' was not declared 'constexpr'" "" }
constexpr auto [ ae, af, ag ] = z;			// { dg-warning "structured bindings only available with" "" { target c++14_down } }
							// { dg-warning "structured binding declaration can be 'constexpr' only with" "" { target c++23_down } .-1 }
							// { dg-error "the value of 'z' is not usable in a constant expression" "" { target *-*-* } .-2 }
							// { dg-error "passing 'const A' as 'this' argument discards qualifiers" "" { target *-*-* } .-3 }
							// { dg-error "call to non-'constexpr' function 'int\\\& A::get\\\(\\\)" "" { target *-*-* } .-4 }
#if __cpp_constinit >= 201907
constinit const auto [ ah, ai, aj ] = z;		// { dg-warning "'constinit' can be applied to structured binding only with" "" { target { c++20 && c++23_down } } }
							// { dg-error "'constinit' variable '<structured bindings>' does not have a constant initializer" "" { target c++20 } .-1 }
							// { dg-error "the value of 'z' is not usable in a constant expression" "" { target c++20 } .-2 }
							// { dg-error "passing 'const A' as 'this' argument discards qualifiers" "" { target c++20 } .-3 }
							// { dg-error "call to non-'constexpr' function 'int\\\& A::get\\\(\\\)" "" { target c++20 } .-4 }
							// { dg-error "'constinit' variable 'ah' does not have a constant initializer" "" { target c++20 } .-5 }
							// { dg-error "'constinit' variable 'ai' does not have a constant initializer" "" { target c++20 } .-6 }
							// { dg-error "'constinit' variable 'aj' does not have a constant initializer" "" { target c++20 } .-7 }
#endif
constexpr auto & [ ak, al, am ] = z;			// { dg-warning "structured bindings only available with" "" { target c++14_down } }
							// { dg-warning "structured binding declaration can be 'constexpr' only with" "" { target c++23_down } .-1 }
							// { dg-error "call to non-'constexpr' function 'int\\\& A::get\\\(\\\)" "" { target *-*-* } .-2 }
#if __cpp_constinit >= 201907
constinit auto & [ an, ao, ap ] = z;			// { dg-warning "'constinit' can be applied to structured binding only with" "" { target { c++20 && c++23_down } } }
							// { dg-error "'constinit' variable 'an' does not have a constant initializer" "" { target c++20 } .-1 }
							// { dg-error "'constinit' variable 'ao' does not have a constant initializer" "" { target c++20 } .-2 }
							// { dg-error "'constinit' variable 'ap' does not have a constant initializer" "" { target c++20 } .-3 }
							// { dg-message "call to non-'constexpr' function 'int\\\& A::get\\\(\\\)" "" { target c++20 } .-4 }
#endif

constexpr C zz = { 42, -42 };
constexpr auto [ aq, ar, as ] = zz;			// { dg-warning "structured bindings only available with" "" { target c++14_down } }
							// { dg-warning "structured binding declaration can be 'constexpr' only with" "" { target c++23_down } .-1 }
							// { dg-error "call to non-'constexpr' function 'const int\\\& C::get\\\(\\\) const" "" { target *-*-* } .-2 }
#if __cpp_constinit >= 201907
constinit const auto [ at, au, av ] = zz;		// { dg-warning "'constinit' can be applied to structured binding only with" "" { target { c++20 && c++23_down } } }
							// { dg-error "call to non-'constexpr' function 'const int\\\& C::get\\\(\\\) const" "" { target c++20 } .-1 }
							// { dg-error "'constinit' variable 'at' does not have a constant initializer" "" { target c++20 } .-2 }
							// { dg-error "'constinit' variable 'au' does not have a constant initializer" "" { target c++20 } .-3 }
							// { dg-error "'constinit' variable 'av' does not have a constant initializer" "" { target c++20 } .-4 }
#endif
constexpr auto & [ aw, ax, ay ] = zz;			// { dg-warning "structured bindings only available with" "" { target c++14_down } }
							// { dg-warning "structured binding declaration can be 'constexpr' only with" "" { target c++23_down } .-1 }
							// { dg-error "call to non-'constexpr' function 'const int\\\& C::get\\\(\\\) const" "" { target *-*-* } .-2 }
#if __cpp_constinit >= 201907
constinit auto & [ az, ba, bb ] = zz;			// { dg-warning "'constinit' can be applied to structured binding only with" "" { target { c++20 && c++23_down } } }
							// { dg-error "'constinit' variable 'az' does not have a constant initializer" "" { target c++20 } .-1 }
							// { dg-error "'constinit' variable 'ba' does not have a constant initializer" "" { target c++20 } .-2 }
							// { dg-error "'constinit' variable 'bb' does not have a constant initializer" "" { target c++20 } .-3 }
							// { dg-message "call to non-'constexpr' function 'const int\\\& C::get\\\(\\\) const" "" { target c++20 } .-4 }
#endif

void
foo ()
{
#if __cpp_constinit >= 201907
  constexpr B a[3] = { { 1, 2, 3, 4 }, { 5, 6, 7, 8 }, { 9, 10, 11, 12 } };
  constinit auto [ b, c, d ] = a;			// { dg-error "'constinit' can only be applied to a variable with static or thread storage duration" "" { target c++20 } }
  constinit auto & [ e, f, g ] = a;			// { dg-error "'constinit' can only be applied to a variable with static or thread storage duration" "" { target c++20 } }
  constinit auto [ h, i, j, k ] = a[1];			// { dg-error "'constinit' can only be applied to a variable with static or thread storage duration" "" { target c++20 } }
  constinit auto & [ l, m, n, o ] = a[2];		// { dg-error "'constinit' can only be applied to a variable with static or thread storage duration" "" { target c++20 } }
#endif
}
