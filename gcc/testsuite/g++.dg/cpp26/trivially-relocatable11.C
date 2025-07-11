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
struct is_replaceable
  : public integral_constant <bool, __builtin_is_replaceable (T)>
{
};

template <typename T>
inline constexpr bool is_trivially_relocatable_v	// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  = __builtin_is_trivially_relocatable (T);		// { dg-warning "variable templates only available with" "" { target c++11_down } .-1 }

template <typename T>
inline constexpr bool is_replaceable_v			// { dg-warning "inline variables are only available with" "" { target c++14_down } }
  = __builtin_is_replaceable (T);			// { dg-warning "variable templates only available with" "" { target c++11_down } .-1 }
}

struct A { A (A &&) = default; A &operator= (A &&) = default; ~A () = default; int a; };

static_assert (std::is_trivially_relocatable_v <A>, "");
static_assert (std::is_replaceable_v <A>, "");

struct B { B (B &&); B &operator= (B &&) = default; ~B () = default; int a; };

static_assert (!std::is_trivially_relocatable_v <B>, "");
static_assert (!std::is_replaceable_v <B>, "");

union C { int a; A b; };

static_assert (std::is_trivially_relocatable_v <C>, "");
static_assert (std::is_replaceable_v <C>, "");

union D { int a; A b; B c; };

static_assert (!std::is_trivially_relocatable_v <D>, "");
static_assert (!std::is_replaceable_v <D>, "");

union E { E (); int a; A b; };

static_assert (std::is_trivially_relocatable_v <E>, "");
static_assert (std::is_replaceable_v <E>, "");

union F { F () = default; int a; A b; };

static_assert (std::is_trivially_relocatable_v <F>, "");
static_assert (std::is_replaceable_v <F>, "");

union G { G (const G &); int a; A b; };

static_assert (!std::is_trivially_relocatable_v <G>, "");
static_assert (!std::is_replaceable_v <G>, "");

union H { H (const H &) = default; int a; A b; };

static_assert (!std::is_trivially_relocatable_v <H>, "");
static_assert (!std::is_replaceable_v <H>, "");

union I { I (I &&); int a; A b; };

static_assert (!std::is_trivially_relocatable_v <I>, "");
static_assert (!std::is_replaceable_v <I>, "");

union J { J (J &&) = default; int a; A b; };

static_assert (!std::is_trivially_relocatable_v <J>, "");
static_assert (!std::is_replaceable_v <J>, "");

union K { K &operator= (const K &); int a; A b; };

static_assert (!std::is_trivially_relocatable_v <K>, "");
static_assert (!std::is_replaceable_v <K>, "");

union L { L &operator= (const L &) = default; int a; A b; };

static_assert (!std::is_trivially_relocatable_v <L>, "");
static_assert (!std::is_replaceable_v <L>, "");

union M { M &operator= (M &&); int a; A b; };

static_assert (!std::is_trivially_relocatable_v <M>, "");
static_assert (!std::is_replaceable_v <M>, "");

union N { N &operator= (N &&) = default; int a; A b; };

static_assert (!std::is_trivially_relocatable_v <N>, "");
static_assert (!std::is_replaceable_v <N>, "");

union O { ~O (); int a; A b; };

static_assert (!std::is_trivially_relocatable_v <O>, "");
static_assert (!std::is_replaceable_v <O>, "");

union P { ~P () = default; int a; A b; };

static_assert (!std::is_trivially_relocatable_v <P>, "");
static_assert (!std::is_replaceable_v <P>, "");

union Q { int a; const A b; };

static_assert (std::is_trivially_relocatable_v <Q>, "");
static_assert (!std::is_replaceable_v <Q>, "");

union S { volatile int a; A b; };

static_assert (std::is_trivially_relocatable_v <S>, "");
static_assert (!std::is_replaceable_v <S>, "");
