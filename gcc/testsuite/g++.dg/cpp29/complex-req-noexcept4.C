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

static_assert (requires { { foo () } noexcept (B <false> {}); });	// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
static_assert (!requires { { foo () } noexcept (B <true> {}); });	// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
static_assert (requires { { bar () } noexcept (B <false> {}); });	// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
static_assert (requires { { bar () } noexcept (B <true> {}); });	// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }

template <typename T, typename U>
concept V = requires (T t) { { t.foo () } noexcept (U {}); };		// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }

static_assert (!V <C, A <false>>);
static_assert (!V <C, A <true>>);
static_assert (!V <D, A <false>>);
static_assert (!V <D, A <true>>);
static_assert (V <C, B <false>>);
static_assert (!V <C, B <true>>);
static_assert (V <D, B <false>>);
static_assert (V <D, B <true>>);
