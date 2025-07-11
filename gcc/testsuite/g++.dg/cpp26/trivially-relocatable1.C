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

struct A {};

static_assert (std::is_trivially_relocatable_v <A>, "");
static_assert (std::is_replaceable_v <A>, "");

struct B {
  B ();
  ~B ();
  B (const B &);
  B (B &&);
  B &operator= (const B &);
  B &operator= (B &&);
};

static_assert (!std::is_trivially_relocatable_v <B>, "");
static_assert (!std::is_replaceable_v <B>, "");

struct C {
  C (C &&) = delete;
  C &operator= (C &&) = delete;
  C () = default;
};

// Note, P2786R13 says it is trivially relocatable, but I think
// it isn't default-movable because overload resolution in both
// cases selects a deleted special member fn.
static_assert (!std::is_trivially_relocatable_v <C>, "");
static_assert (!std::is_replaceable_v <C>, "");

struct D : A {};

static_assert (std::is_trivially_relocatable_v <D>, "");
static_assert (std::is_replaceable_v <D>, "");

struct E : virtual A {};

static_assert (!std::is_trivially_relocatable_v <E>, "");
static_assert (std::is_replaceable_v <E>, "");

struct F trivially_relocatable_if_eligible : virtual A {};

static_assert (!std::is_trivially_relocatable_v <F>, "");
static_assert (std::is_replaceable_v <F>, "");

struct G { B data; };

static_assert (!std::is_trivially_relocatable_v <G>, "");
static_assert (!std::is_replaceable_v <G>, "");

struct H { ~H () = default; };

static_assert (std::is_trivially_relocatable_v <H>, "");
static_assert (std::is_replaceable_v <H>, "");

struct I { ~I (); };
I::~I () = default;

static_assert (!std::is_trivially_relocatable_v <I>, "");
static_assert (!std::is_replaceable_v <I>, "");

struct J { virtual ~J () = default; };

// Note, P2786R13 says otherwise for both, but that looks like
// a bug in the paper, it otherwise says that polymorphic types
// can be both trivially relocatable and replaceable.
static_assert (std::is_trivially_relocatable_v <J>, "");
static_assert (std::is_replaceable_v <J>, "");

struct K { ~K () = delete; };

static_assert (!std::is_trivially_relocatable_v <K>, "");
static_assert (!std::is_replaceable_v <K>, "");

struct L { L (L &&) = default; };

// Note, P2786R13 says otherwise for both, but that looks like
// a bug in the paper to me.  While move ctor is trivial here,
// copy assignment operator is implicitly declared as deleted
// and move assignent operator is not declared.
static_assert (!std::is_trivially_relocatable_v <L>, "");
static_assert (!std::is_replaceable_v <L>, "");

struct M { M (M &&); };
M::M (M &&) = default;

static_assert (!std::is_trivially_relocatable_v <M>, "");
static_assert (!std::is_replaceable_v <M>, "");

struct N { N (N &&) = delete; };

static_assert (!std::is_trivially_relocatable_v <N>, "");
static_assert (!std::is_replaceable_v <N>, "");
