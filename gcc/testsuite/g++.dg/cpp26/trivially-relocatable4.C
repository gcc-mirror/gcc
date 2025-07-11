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

struct A replaceable_if_eligible {
  ~A () = delete;
  A (A &&) = default;
  A &operator= (A &&) = default;
};

static_assert (!std::is_trivially_relocatable_v <A>, "");
static_assert (!std::is_replaceable_v <A>, "");

struct B replaceable_if_eligible { B (const B &) = delete; };

static_assert (!std::is_trivially_relocatable_v <B>, "");
static_assert (!std::is_replaceable_v <B>, "");

template <typename T>
struct C replaceable_if_eligible : virtual T {};

static_assert (!std::is_trivially_relocatable_v <C<A>>, "");
static_assert (!std::is_trivially_relocatable_v <C<B>>, "");
static_assert (!std::is_replaceable_v <C<A>>, "");
static_assert (!std::is_replaceable_v <C<B>>, "");

template <typename T>
struct D { int s; T t; };

static_assert (!std::is_trivially_relocatable_v <C<D<int>>>, "");
static_assert (std::is_replaceable_v <C<D<int>>>, "");

struct E trivially_relocatable_if_eligible replaceable_if_eligible {
  E (E &&);
  E &operator= (E &&) = default;
};

static_assert (std::is_trivially_relocatable_v <E>, "");
static_assert (std::is_replaceable_v <E>, "");

struct F trivially_relocatable_if_eligible replaceable_if_eligible {
  F (F &&) = default;
  F &operator= (F &&);
};

static_assert (std::is_trivially_relocatable_v <F>, "");
static_assert (std::is_replaceable_v <F>, "");

struct G replaceable_if_eligible { G (G const &) = default; };

static_assert (std::is_trivially_relocatable_v <G>, "");
static_assert (std::is_replaceable_v <G>, "");

struct H { H (H const &) = default; };

static_assert (std::is_trivially_relocatable_v <H>, "");
static_assert (std::is_replaceable_v <H>, "");

struct I replaceable_if_eligible { I &operator= (const I &) = default; };

static_assert (std::is_trivially_relocatable_v <I>, "");
static_assert (std::is_replaceable_v <I>, "");

struct J { J &operator= (J const &) = default; };

static_assert (std::is_trivially_relocatable_v <J>, "");
static_assert (std::is_replaceable_v <J>, "");

struct K { K (const K &) = delete; };

static_assert (!std::is_trivially_relocatable_v <K>, "");
static_assert (!std::is_replaceable_v <K>, "");

struct L { L (L&&) = delete; };

static_assert (!std::is_trivially_relocatable_v <L>, "");
static_assert (!std::is_replaceable_v <L>, "");

struct M { M operator= (M); };

static_assert (!std::is_trivially_relocatable_v <M>, "");
static_assert (!std::is_replaceable_v <M>, "");

struct N { N operator= (N &&); };

static_assert (!std::is_trivially_relocatable_v <N>, "");
static_assert (!std::is_replaceable_v <N>, "");
