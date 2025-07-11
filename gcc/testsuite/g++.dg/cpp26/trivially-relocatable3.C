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

class A {};

struct B { ~B (); };

class C trivially_relocatable_if_eligible { C (C &&); };

template <typename T>
class D trivially_relocatable_if_eligible : T {};
D<A> a;
D<B> b;

static_assert (std::is_trivially_relocatable_v <D<A>>, "");
static_assert (!std::is_trivially_relocatable_v <D<B>>, "");
static_assert (std::is_replaceable_v <D<A>>, "");
static_assert (!std::is_replaceable_v <D<B>>, "");

struct E { E (E &&) = delete; };

static_assert (!std::is_trivially_relocatable_v <E>, "");
static_assert (!std::is_replaceable_v <E>, "");

struct F { F (const F &) = delete; };

static_assert (!std::is_trivially_relocatable_v <F>, "");
static_assert (!std::is_replaceable_v <F>, "");

struct G { G &operator= (G &&) = delete; };

static_assert (!std::is_trivially_relocatable_v <G>, "");
static_assert (!std::is_replaceable_v <G>, "");

struct H { ~H () = delete; };

static_assert (!std::is_trivially_relocatable_v <H>, "");
static_assert (!std::is_replaceable_v <H>, "");

union U { C u; };

static_assert (std::is_trivially_relocatable_v <U>, "");
static_assert (!std::is_replaceable_v <U>, "");

template <typename T>
struct I { int s; T t; };

static_assert (std::is_trivially_relocatable_v <I<int>>, "");
static_assert (std::is_trivially_relocatable_v <I<volatile int>>, "");
static_assert (!std::is_trivially_relocatable_v <I<const int>>, "");
static_assert (!std::is_trivially_relocatable_v <I<const int &>>, "");
static_assert (!std::is_trivially_relocatable_v <I<int &>>, "");
static_assert (std::is_trivially_relocatable_v <I<int [2]>>, "");
static_assert (!std::is_trivially_relocatable_v <I<const int [2]>>, "");
static_assert (std::is_trivially_relocatable_v <I<int []>>, "");
static_assert (std::is_replaceable_v <I<int>>, "");
static_assert (!std::is_replaceable_v <I<volatile int>>, "");
static_assert (!std::is_replaceable_v <I<const int>>, "");
static_assert (!std::is_replaceable_v <I<const int &>>, "");
static_assert (!std::is_replaceable_v <I<int &>>, "");
static_assert (std::is_replaceable_v <I<int [2]>>, "");
static_assert (!std::is_replaceable_v <I<const int [2]>>, "");

template <typename T>
struct J trivially_relocatable_if_eligible { int s; T t; };

static_assert (std::is_trivially_relocatable_v <J<int>>, "");
static_assert (std::is_trivially_relocatable_v <J<volatile int>>, "");
static_assert (std::is_trivially_relocatable_v <J<const int>>, "");
static_assert (std::is_trivially_relocatable_v <J<const int &>>, "");
static_assert (std::is_trivially_relocatable_v <J<int &>>, "");
static_assert (std::is_trivially_relocatable_v <J<int [2]>>, "");
static_assert (std::is_trivially_relocatable_v <J<const int [2]>>, "");
static_assert (std::is_trivially_relocatable_v <J<int []>>, "");
static_assert (std::is_replaceable_v <J<int>>, "");
static_assert (!std::is_replaceable_v <J<volatile int>>, "");
static_assert (!std::is_replaceable_v <J<const int>>, "");
static_assert (!std::is_replaceable_v <J<const int &>>, "");
static_assert (!std::is_replaceable_v <J<int &>>, "");
static_assert (std::is_replaceable_v <J<int [2]>>, "");
static_assert (!std::is_replaceable_v <J<const int [2]>>, "");
static_assert (std::is_replaceable_v <J<int []>>, "");

struct K { K (K &&) = delete; };

static_assert (!std::is_trivially_relocatable_v <K>, "");
static_assert (!std::is_replaceable_v <K>, "");

struct L { L (const L &) = delete; };

static_assert (!std::is_trivially_relocatable_v <L>, "");
static_assert (!std::is_replaceable_v <L>, "");

struct M { M &operator= (M &&) = delete; };

static_assert (!std::is_trivially_relocatable_v <M>, "");
static_assert (!std::is_replaceable_v <M>, "");

struct N { N (N &&) = default; N &operator= (N &&) = default; };

static_assert (std::is_trivially_relocatable_v <N>, "");
static_assert (std::is_replaceable_v <N>, "");

struct O {
  O (const O &) = default;
  O (O &&) = default;
  O &operator= (O &&) = default;
};

static_assert (std::is_trivially_relocatable_v <O>, "");
static_assert (std::is_replaceable_v <O>, "");

struct P { P (P &&) = default; P &operator= (P &&) = default; };

static_assert (std::is_trivially_relocatable_v <P>, "");
static_assert (std::is_replaceable_v <P>, "");

struct Q { Q (Q &&) {} };

static_assert (!std::is_trivially_relocatable_v <Q>, "");
static_assert (!std::is_replaceable_v <Q>, "");

struct R { R (const R &) {} };

static_assert (!std::is_trivially_relocatable_v <R>, "");
static_assert (!std::is_replaceable_v <R>, "");

struct S { S &operator= (const S &) { return *this; }; };

static_assert (!std::is_trivially_relocatable_v <S>, "");
static_assert (!std::is_replaceable_v <S>, "");

struct T {};

static_assert (std::is_trivially_relocatable_v <T>, "");
static_assert (std::is_replaceable_v <T>, "");

struct V replaceable_if_eligible {};

static_assert (std::is_trivially_relocatable_v <V>, "");
static_assert (std::is_replaceable_v <V>, "");

struct W { template <typename U> W (const U &) = delete; };

static_assert (std::is_trivially_relocatable_v <W>, "");
static_assert (std::is_replaceable_v <W>, "");

template <typename T>
struct X : T {};

static_assert (!std::is_trivially_relocatable_v <X<Q>>, "");
static_assert (!std::is_replaceable_v <X<Q>>, "");

template <typename T>
struct Y : virtual T {};

static_assert (!std::is_trivially_relocatable_v <Y<I<int>>>, "");
static_assert (!std::is_trivially_relocatable_v <Y<I<const int>>>, "");
static_assert (!std::is_trivially_relocatable_v <Y<Q>>, "");
static_assert (std::is_replaceable_v <Y<I<int>>>, "");
static_assert (!std::is_replaceable_v <Y<I<const int>>>, "");
static_assert (!std::is_replaceable_v <Y<Q>>, "");

struct Z {
  virtual ~Z () = default;
  Z (Z &&) = default;
  Z &operator= (Z &&) = default;
};

static_assert (std::is_trivially_relocatable_v <Z>, "");
static_assert (std::is_replaceable_v <Z>, "");
