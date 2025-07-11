// P2786R13 - C++26 Trivial Relocatability
// { dg-do compile { target c++11 } }
// { dg-options "" }
// { dg-additional-options "-pedantic" { target c++17 } }

#if __cpp_trivial_relocatability < 202502L
#define trivially_relocatable_if_eligible __trivially_relocatable_if_eligible
#define replaceable_if_eligible __replaceable_if_eligible
#endif

namespace std
{
template <typename T, T v>
struct integral_constant
{
  static constexpr T value = v;
};

template <typename>
struct is_trivially_relocatable;

template <typename>
struct is_replaceable;

template<typename T>
struct is_trivially_relocatable
  : public integral_constant <bool, __builtin_is_trivially_relocatable (T)>
{
};

template<typename T>
struct is_nothrow_relocatable
  : public integral_constant <bool, __builtin_is_nothrow_relocatable (T)>
{
};

template<typename T>
struct is_replaceable
  : public integral_constant <bool, __builtin_is_replaceable (T)>
{
};

template <typename T>
inline constexpr bool is_trivially_relocatable_v	// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  = __builtin_is_trivially_relocatable (T);		// { dg-warning "variable templates only available with" "" { target c++11_down } .-1 }

template <typename T>
inline constexpr bool is_nothrow_relocatable_v		// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  = __builtin_is_nothrow_relocatable (T);		// { dg-warning "variable templates only available with" "" { target c++11_down } .-1 }

template <typename T>
inline constexpr bool is_replaceable_v			// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  = __builtin_is_replaceable (T);			// { dg-warning "variable templates only available with" "" { target c++11_down } .-1 }
}

struct A { A (A &&) = default; A &operator= (A &&) = default; ~A () = default; int a; };

static_assert (std::is_trivially_relocatable_v <A>, "");
static_assert (std::is_nothrow_relocatable_v <A>, "");
static_assert (std::is_replaceable_v <A>, "");

struct B { B (B &&); B &operator= (B &&) = default; ~B () = default; int a; };

static_assert (!std::is_trivially_relocatable_v <B>, "");
static_assert (!std::is_nothrow_relocatable_v <B>, "");
static_assert (!std::is_replaceable_v <B>, "");

struct C { C (C &&) = default; C &operator= (C &&); ~C () = default; int a; };

static_assert (!std::is_trivially_relocatable_v <C>, "");
static_assert (std::is_nothrow_relocatable_v <C>, "");
static_assert (!std::is_replaceable_v <C>, "");

struct D { D (D &&) = delete; D &operator= (D &&) = default; int a; };

static_assert (!std::is_trivially_relocatable_v <D>, "");
static_assert (!std::is_nothrow_relocatable_v <D>, "");
static_assert (!std::is_replaceable_v <D>, "");

struct E { E (E &&) = default; E &operator= (E &&) = delete; int a; };

static_assert (!std::is_trivially_relocatable_v <E>, "");
static_assert (std::is_nothrow_relocatable_v <E>, "");
static_assert (!std::is_replaceable_v <E>, "");

struct F { F (F &&) = default; F &operator= (F &&) = default; ~F () = delete; int a; };

static_assert (!std::is_trivially_relocatable_v <F>, "");
static_assert (!std::is_nothrow_relocatable_v <F>, "");
static_assert (!std::is_replaceable_v <F>, "");

struct G { G (const G &) = default; G &operator= (const G &) = default; int a; };

static_assert (std::is_trivially_relocatable_v <G>, "");
static_assert (std::is_nothrow_relocatable_v <G>, "");
static_assert (std::is_replaceable_v <G>, "");

struct H { H (const H &); H &operator= (const H &) = default; int a; };

static_assert (!std::is_trivially_relocatable_v <H>, "");
static_assert (!std::is_nothrow_relocatable_v <H>, "");
static_assert (!std::is_replaceable_v <H>, "");

struct I { I (const I &) = default; I &operator= (const I &); ~I () = default; int a; };

static_assert (!std::is_trivially_relocatable_v <I>, "");
static_assert (std::is_nothrow_relocatable_v <I>, "");
static_assert (!std::is_replaceable_v <I>, "");

struct J { J (const J &) = delete; J &operator= (const J &) = default; int a; };

static_assert (!std::is_trivially_relocatable_v <J>, "");
static_assert (!std::is_nothrow_relocatable_v <J>, "");
static_assert (!std::is_replaceable_v <J>, "");

struct K { K (const K &) = default; K &operator= (const K &) = delete; int a; };

static_assert (!std::is_trivially_relocatable_v <K>, "");
static_assert (std::is_nothrow_relocatable_v <K>, "");
static_assert (!std::is_replaceable_v <K>, "");

struct M;
struct L { L (L &&) = default; L (M &&); L &operator= (L &&) = default; int a; };

static_assert (std::is_trivially_relocatable_v <L>, "");
static_assert (std::is_nothrow_relocatable_v <L>, "");
static_assert (std::is_replaceable_v <L>, "");

struct M : public L { using L::L; M (const M &); M &operator= (M &&) = default; int b; };

static_assert (!std::is_trivially_relocatable_v <M>, "");
static_assert (!std::is_nothrow_relocatable_v <M>, "");
static_assert (!std::is_replaceable_v <M>, "");

struct O;
struct N { N (N &&) = default; N &operator= (N &&) = default; N &operator= (O &&); int a; };

static_assert (std::is_trivially_relocatable_v <N>, "");
static_assert (std::is_nothrow_relocatable_v <N>, "");
static_assert (std::is_replaceable_v <N>, "");

struct O : public N { using N::operator=; O (O &&) = default; int b; };

static_assert (!std::is_trivially_relocatable_v <O>, "");
static_assert (std::is_nothrow_relocatable_v <O>, "");
static_assert (!std::is_replaceable_v <O>, "");

struct Q;
struct P { template <typename T> P (T &&) {} };

static_assert (std::is_trivially_relocatable_v <P>, "");
static_assert (std::is_nothrow_relocatable_v <P>, "");
static_assert (std::is_replaceable_v <P>, "");

struct Q : public P { using P::P; Q (const Q &); };

static_assert (!std::is_trivially_relocatable_v <Q>, "");
static_assert (!std::is_nothrow_relocatable_v <Q>, "");
static_assert (!std::is_replaceable_v <Q>, "");

struct S;
struct R { R (const R &) = default; R (const M &); R &operator= (R &&) = default; int a; };

static_assert (std::is_trivially_relocatable_v <R>, "");
static_assert (std::is_nothrow_relocatable_v <R>, "");
static_assert (std::is_replaceable_v <R>, "");

struct S : public R { using R::R; S &operator= (S &&) = default; int b; };

static_assert (!std::is_trivially_relocatable_v <S>, "");
static_assert (!std::is_nothrow_relocatable_v <S>, "");
static_assert (!std::is_replaceable_v <S>, "");

struct T { T (T &&) = default; T &operator= (T &&) = default; ~T (); int a; };

static_assert (!std::is_trivially_relocatable_v <T>, "");
static_assert (std::is_nothrow_relocatable_v <T>, "");
static_assert (!std::is_replaceable_v <T>, "");

struct U { U (const U &) = default; U &operator= (const U &) = default; ~U (); int a; };

static_assert (!std::is_trivially_relocatable_v <U>, "");
static_assert (std::is_nothrow_relocatable_v <U>, "");
static_assert (!std::is_replaceable_v <U>, "");

struct V { public: V (); private: V (V &&) = default; V &operator= (V &&) = default; ~V () = default; int a; };

static_assert (std::is_trivially_relocatable_v <V>, "");
static_assert (std::is_nothrow_relocatable_v <V>, "");
static_assert (std::is_replaceable_v <V>, "");
