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

static_assert (std::is_trivially_relocatable_v <A>, "");
static_assert (std::is_replaceable_v <A>, "");
static_assert (std::is_trivially_relocatable <A>::value, "");
static_assert (std::is_replaceable <A>::value, "");

struct B { ~B (); };
static B z;

static_assert (!std::is_trivially_relocatable_v <B>, "");
static_assert (!std::is_replaceable_v <B>, "");
static_assert (!std::is_trivially_relocatable <B>::value, "");
static_assert (!std::is_replaceable <B>::value, "");

class C trivially_relocatable_if_eligible {};

static_assert (std::is_trivially_relocatable_v <C>, "");
static_assert (std::is_replaceable_v <C>, "");

class D trivially_relocatable_if_eligible : A {};

static_assert (std::is_trivially_relocatable_v <D>, "");
static_assert (std::is_replaceable_v <D>, "");

class E trivially_relocatable_if_eligible {
  int a;
  void *b;
  int c[3];
  A d[3];
  B &e = z;
};

static_assert (std::is_trivially_relocatable_v <E>, "");
static_assert (!std::is_replaceable_v <E>, "");

class F trivially_relocatable_if_eligible : A {};

static_assert (std::is_trivially_relocatable_v <F>, "");
static_assert (std::is_replaceable_v <F>, "");

class G trivially_relocatable_if_eligible : virtual A {};

static_assert (!std::is_trivially_relocatable_v <G>, "");
static_assert (std::is_replaceable_v <G>, "");

class H trivially_relocatable_if_eligible : B {};

static_assert (!std::is_trivially_relocatable_v <H>, "");
static_assert (!std::is_replaceable_v <H>, "");

class I trivially_relocatable_if_eligible { I (I &&); };

static_assert (std::is_trivially_relocatable_v <I>, "");
static_assert (!std::is_replaceable_v <I>, "");

class J trivially_relocatable_if_eligible { ~J (); };

static_assert (std::is_trivially_relocatable_v <J>, "");
static_assert (!std::is_replaceable_v <J>, "");

class K trivially_relocatable_if_eligible {
  B a;
  B b[1];
  const B c;
  const B d[1];
};

static_assert (!std::is_trivially_relocatable_v <K>, "");
static_assert (!std::is_replaceable_v <K>, "");

class L trivially_relocatable_if_eligible: virtual A, B { B a; };

static_assert (!std::is_trivially_relocatable_v <L>, "");
static_assert (!std::is_replaceable_v <L>, "");

static_assert (!std::is_trivially_relocatable_v <void>, "");
static_assert (!std::is_trivially_relocatable_v <const void>, "");
static_assert (std::is_trivially_relocatable_v <int>, "");
static_assert (std::is_trivially_relocatable_v <const int>, "");
static_assert (std::is_trivially_relocatable_v <char>, "");
static_assert (std::is_trivially_relocatable_v <char const volatile>, "");
static_assert (std::is_trivially_relocatable_v <unsigned long long>, "");
static_assert (std::is_trivially_relocatable_v <void *>, "");
static_assert (std::is_trivially_relocatable_v <const int *>, "");
static_assert (!std::is_trivially_relocatable_v <int &>, "");
static_assert (!std::is_trivially_relocatable_v <A &>, "");
static_assert (std::is_trivially_relocatable_v <const A>, "");
static_assert (std::is_trivially_relocatable_v <A [1]>, "");
static_assert (std::is_trivially_relocatable_v <A []>, "");
static_assert (!std::is_replaceable_v <void>, "");
static_assert (!std::is_replaceable_v <const void>, "");
static_assert (std::is_replaceable_v <int>, "");
static_assert (!std::is_replaceable_v <const int>, "");
static_assert (std::is_replaceable_v <char>, "");
static_assert (!std::is_replaceable_v <char const volatile>, "");
static_assert (std::is_replaceable_v <unsigned long long>, "");
static_assert (std::is_replaceable_v <void *>, "");
static_assert (std::is_replaceable_v <const int *>, "");
static_assert (!std::is_replaceable_v <int &>, "");
static_assert (!std::is_replaceable_v <A &>, "");
static_assert (!std::is_replaceable_v <const A>, "");
static_assert (std::is_replaceable_v <A [1]>, "");
static_assert (std::is_replaceable_v <A []>, "");

struct M { const int i; };

static_assert (!std::is_trivially_relocatable_v <M>, "");
static_assert (!std::is_replaceable_v <M>, "");

struct N trivially_relocatable_if_eligible { const int i; };

static_assert (std::is_trivially_relocatable_v <N>, "");
static_assert (!std::is_replaceable_v <N>, "");

struct O { ~O (); };

static_assert (!std::is_trivially_relocatable_v <O>, "");
static_assert (!std::is_replaceable_v <O>, "");

struct P { ~P () = default; };

static_assert (std::is_trivially_relocatable_v <P>, "");
static_assert (std::is_replaceable_v <P>, "");

struct Q { Q (Q &&); Q (const Q &) = default; };

static_assert (!std::is_trivially_relocatable_v <Q>, "");
static_assert (!std::is_replaceable_v <Q>, "");

struct R { R (R &&); };

static_assert (!std::is_trivially_relocatable_v <R>, "");
static_assert (!std::is_replaceable_v <R>, "");

struct S { S (S &&) = default; };

static_assert (!std::is_trivially_relocatable_v <S>, "");
static_assert (!std::is_replaceable_v <S>, "");

struct T { T (T &&) = default; T &operator= (T &&) = default; };

static_assert (std::is_trivially_relocatable_v <T>, "");
static_assert (std::is_replaceable_v <T>, "");

struct U { U (const U &); };

static_assert (!std::is_trivially_relocatable_v <U>, "");
static_assert (!std::is_replaceable_v <U>, "");

struct V { V (const V&) = default; };

static_assert (std::is_trivially_relocatable_v <V>, "");
static_assert (std::is_replaceable_v <V>, "");

struct W { W (W &&) = delete; W (const W &) = default; };

static_assert (!std::is_trivially_relocatable_v <W>, "");
static_assert (!std::is_replaceable_v <W>, "");
