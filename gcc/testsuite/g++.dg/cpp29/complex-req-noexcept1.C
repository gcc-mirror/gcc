// C++29 P3822R2 - Conditional noexcept specifiers in compound requirements
// { dg-do compile { target c++20 } }
// { dg-options "" }

int foo ();
int bar () noexcept;
struct A { int foo (); };
struct B { int foo () noexcept; };
struct C { int foo (); char c[42]; };
struct D { int foo () noexcept; char c[42]; };
struct E { long foo (); };
struct F { long foo () noexcept; };
struct G { long foo (); char c[42]; };
struct H { long foo () noexcept; char c[42]; };
template <bool B>
struct J { constexpr operator bool () { return B; }; };
template <typename T, typename U>
constexpr bool s = false;
template <typename T>
constexpr bool s <T, T> = true;
template <typename T, typename U>
concept S = s <T, U>;

static_assert (!requires { { foo () } noexcept; });
static_assert (!requires { { foo () } noexcept (true); });	// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
static_assert (requires { { foo () } noexcept (false); });	// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
static_assert (!requires { { foo () } noexcept (42 == 42); });	// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
static_assert (requires { { foo () } noexcept (42 != 42); });	// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
static_assert (!requires { { foo () } noexcept (J <true> {}); });// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
static_assert (requires { { foo () } noexcept (J <false> {}); });// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
static_assert (requires { { bar () } noexcept; });
static_assert (requires { { bar () } noexcept (true); });	// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
static_assert (requires { { bar () } noexcept (false); });	// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
static_assert (requires { { bar () } noexcept (42 == 42); });	// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
static_assert (requires { { bar () } noexcept (42 != 42); });	// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
static_assert (requires { { bar () } noexcept (J <true> {}); });// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
static_assert (requires { { bar () } noexcept (J <false> {}); });// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }

template <typename T, bool B>
concept V = requires (T t) { { t.foo () } noexcept (B); };	// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }

static_assert (V <A, false>);
static_assert (!V <A, true>);
static_assert (V <B, false>);
static_assert (V <B, true>);

template <typename T>
concept W = requires (T t) {
  { t.foo () } noexcept (sizeof (T) > sizeof (A)) -> S <int>;	// { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }
};

static_assert (W <A>);
static_assert (W <B>);
static_assert (!W <C>);
static_assert (W <D>);
static_assert (!W <E>);
static_assert (!W <F>);
static_assert (!W <G>);
static_assert (!W <H>);

template <typename T>
struct I { static constexpr bool i = sizeof (T) > sizeof (A); };
template <typename T>
concept Z = requires (T t) { { t.foo () } noexcept (I <T>::i); }; // { dg-warning "conditional 'noexcept' in compound requirement only available with" "" { target c++26_down } }

static_assert (Z <A>);
static_assert (Z <B>);
static_assert (!Z <C>);
static_assert (Z <D>);
static_assert (Z <E>);
static_assert (Z <F>);
static_assert (!Z <G>);
static_assert (Z <H>);
