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

struct A replaceable_if_eligible { A (A &&); A &operator= (A &&); ~A (); int a; };

static_assert (std::is_replaceable_v <A>, "");
static_assert (!std::is_replaceable_v <const A>, "");
static_assert (!std::is_replaceable_v <A volatile>, "");
static_assert (!std::is_replaceable_v <const A volatile>, "");

struct B { B (B &&); B &operator= (B &&); ~B (); int a; };

static_assert (!std::is_replaceable_v <B>, "");

struct C replaceable_if_eligible : public A { C (C &&); C &operator= (C &&); ~C (); int a; };

static_assert (std::is_replaceable_v <C>, "");

struct D replaceable_if_eligible : public B { D (D &&); D &operator= (D &&); ~D (); int a; };

static_assert (!std::is_replaceable_v <D>, "");

struct E replaceable_if_eligible { E (E &&); E &operator= (E &&); ~E (); A a; };

static_assert (std::is_replaceable_v <E>, "");

struct F replaceable_if_eligible { F (F &&); F &operator= (F &&); ~F (); B a; };

static_assert (!std::is_replaceable_v <F>, "");

struct G replaceable_if_eligible { G (G &&); G &operator= (G &&); ~G () = delete; int a; };

static_assert (!std::is_replaceable_v <G>, "");

struct H replaceable_if_eligible : virtual A { H (H &&); H &operator= (H &&); ~H (); int a; };

static_assert (std::is_replaceable_v <H>, "");

struct I replaceable_if_eligible { I (I &&) = delete; I &operator= (I &&); ~I (); int a; };

static_assert (!std::is_replaceable_v <I>, "");

struct J replaceable_if_eligible { J (J &&); J &operator= (J &&) = delete; ~J (); int a; };

static_assert (!std::is_replaceable_v <J>, "");

struct K replaceable_if_eligible { K (const K &) = delete; K &operator= (K &&); ~K (); int a; };

static_assert (!std::is_replaceable_v <K>, "");

struct L replaceable_if_eligible { L (L &&); L &operator= (const L &) = delete; ~L (); int a; };

static_assert (!std::is_replaceable_v <L>, "");

struct M replaceable_if_eligible { M (); private: M (M &&); M &operator= (M &&); ~M (); int a; };

static_assert (std::is_replaceable_v <M>, "");

struct N replaceable_if_eligible { N (N &&); N &operator= (N &&); ~N (); const A a; };

static_assert (!std::is_replaceable_v <N>, "");

struct O replaceable_if_eligible { O (O &&); O &operator= (O &&); ~O (); volatile A a; };

static_assert (!std::is_replaceable_v <O>, "");

struct P replaceable_if_eligible { P (P &&); P &operator= (P &&); ~P (); const volatile A a; };

static_assert (!std::is_replaceable_v <P>, "");

struct Q replaceable_if_eligible { Q (Q &&); Q &operator= (Q &&); ~Q (); union { A a; int b; char c; }; };

static_assert (!std::is_replaceable_v <Q>, "");

struct R replaceable_if_eligible { R (R &&); R &operator= (R &&); ~R (); union { int a; B b; short c; }; };

static_assert (!std::is_replaceable_v <R>, "");

struct S replaceable_if_eligible { S (S &&); S &operator= (S &&); ~S (); union { int a; const int b; short c; }; };

static_assert (!std::is_replaceable_v <S>, "");

struct T replaceable_if_eligible { T (T &&); T &operator= (T &&); ~T () = default; int a; };

static_assert (std::is_replaceable_v <T>, "");

struct U replaceable_if_eligible { U (U &&); U &operator= (U &&); ~U (); union { T a; int b; char c; }; };

static_assert (!std::is_replaceable_v <U>, "");

struct V replaceable_if_eligible { V (V &&); V &operator= (V &&); ~V (); union { unsigned long long a; int b; char c; }; };

static_assert (std::is_replaceable_v <V>, "");
