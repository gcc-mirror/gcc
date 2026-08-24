// C++29 P3822R2 - Conditional noexcept specifiers in compound requirements
// { dg-do compile { target c++20 } }
// { dg-options "" }

int foo ();
int bar () noexcept;
template <bool N>
struct A {};
template <bool N>
struct B { constexpr operator bool () { return N; }; };
struct C { int foo (); };
struct D { int foo () noexcept (true); };

bool a = requires { { foo () } noexcept (A <false> {}); };		// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
// { dg-error "could not convert 'A<false>\\\(\\\)' from 'A<false>' to 'bool'" "" { target *-*-* } .-1 }
bool b = requires { { foo () } noexcept (A <true> {}); };		// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
// { dg-error "could not convert 'A<true>\\\(\\\)' from 'A<true>' to 'bool'" "" { target *-*-* } .-1 }

template <typename T, typename U>
concept V = requires (T t) { { t.foo () } noexcept (U {}); };		// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
// { dg-error "could not convert 'A<false>\\\(\\\)' from 'A<false>' to 'bool'" "" { target *-*-* } .-1 }
// { dg-error "could not convert 'A<true>\\\(\\\)' from 'A<true>' to 'bool'" "" { target *-*-* } .-2 }

static_assert (V <C, A <false>>);					// { dg-error "static assertion failed" }
static_assert (V <C, A <true>>);					// { dg-error "static assertion failed" }
static_assert (V <D, A <false>>);					// { dg-error "static assertion failed" }
static_assert (V <D, A <true>>);					// { dg-error "static assertion failed" }
