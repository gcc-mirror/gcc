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

struct A trivially_relocatable_if_eligible { A (A &&); A &operator= (A &&); ~A (); int a; };

static_assert (std::is_trivially_relocatable_v <A>, "");
static_assert (std::is_nothrow_relocatable_v <A>, "");

struct B { B (B &&); B &operator= (B &&); ~B (); int a; };

static_assert (!std::is_trivially_relocatable_v <B>, "");
static_assert (!std::is_nothrow_relocatable_v <B>, "");

struct C trivially_relocatable_if_eligible : public A { C (C &&); C &operator= (C &&); ~C (); int a; };

static_assert (std::is_trivially_relocatable_v <C>, "");
static_assert (std::is_nothrow_relocatable_v <C>, "");

struct D trivially_relocatable_if_eligible : public B { D (D &&); D &operator= (D &&); ~D (); int a; };

static_assert (!std::is_trivially_relocatable_v <D>, "");
static_assert (!std::is_nothrow_relocatable_v <D>, "");

struct E trivially_relocatable_if_eligible { E (E &&); E &operator= (E &&); ~E (); A a; };

static_assert (std::is_trivially_relocatable_v <E>, "");
static_assert (std::is_nothrow_relocatable_v <E>, "");

struct F trivially_relocatable_if_eligible { F (F &&) noexcept; F &operator= (F &&); ~F (); B a; };

static_assert (!std::is_trivially_relocatable_v <F>, "");
static_assert (std::is_nothrow_relocatable_v <F>, "");

struct G trivially_relocatable_if_eligible { G (G &&); G &operator= (G &&); ~G () = delete; int a; };

static_assert (!std::is_trivially_relocatable_v <G>, "");
static_assert (!std::is_nothrow_relocatable_v <G>, "");

struct H trivially_relocatable_if_eligible : virtual A { H (H &&); H &operator= (H &&); ~H (); int a; };

static_assert (!std::is_trivially_relocatable_v <H>, "");
static_assert (!std::is_nothrow_relocatable_v <H>, "");

struct I trivially_relocatable_if_eligible { I (I &&); I &operator= (I &&); ~I (); A &a; int &b; };

static_assert (std::is_trivially_relocatable_v <I>, "");
static_assert (std::is_nothrow_relocatable_v <I>, "");

struct J trivially_relocatable_if_eligible { J (J &&); J &operator= (J &&); ~J (); B &a; int &b; };

static_assert (std::is_trivially_relocatable_v <J>, "");
static_assert (std::is_nothrow_relocatable_v <J>, "");

struct K trivially_relocatable_if_eligible { K (K &&) noexcept; K &operator= (K &&); ~K (); union { A a; int b; char c; }; };

static_assert (!std::is_trivially_relocatable_v <K>, "");
static_assert (std::is_nothrow_relocatable_v <K>, "");

struct L trivially_relocatable_if_eligible { L (L &&); L &operator= (L &&); ~L (); union { int a; B b; short c; }; };

static_assert (!std::is_trivially_relocatable_v <L>, "");
static_assert (!std::is_nothrow_relocatable_v <L>, "");

struct M trivially_relocatable_if_eligible { M (M &&); M &operator= (M &&); ~M () = default; int a; };

static_assert (std::is_trivially_relocatable_v <M>, "");
static_assert (std::is_nothrow_relocatable_v <M>, "");

struct N trivially_relocatable_if_eligible { N (N &&); N &operator= (N &&); ~N (); union { M a; int b; char c; }; };

static_assert (std::is_trivially_relocatable_v <N>, "");
static_assert (std::is_nothrow_relocatable_v <N>, "");

struct O trivially_relocatable_if_eligible { O (O &&); O &operator= (O &&); ~O (); union { unsigned long long a; int b; char c; }; };

static_assert (std::is_trivially_relocatable_v <O>, "");
static_assert (std::is_nothrow_relocatable_v <O>, "");

struct P { P (P &&) noexcept; P &operator= (P &&); ~P (); int a; };

static_assert (!std::is_trivially_relocatable_v <P>, "");
static_assert (std::is_nothrow_relocatable_v <P>, "");
