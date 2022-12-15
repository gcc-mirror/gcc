// P0466R5
// { dg-do compile { target c++20 } }

namespace std
{
template <typename T, T v>
struct integral_constant
{
  static constexpr T value = v;
};

template <typename, typename>
struct is_layout_compatible;

template<typename T, typename U>
struct is_layout_compatible
  : public integral_constant <bool, __is_layout_compatible (T, U)>
{
};

template <typename T, typename U>
inline constexpr bool is_layout_compatible_v = __is_layout_compatible (T, U);
}

// Weird cases.
struct S {};
struct T {};
struct I { int a; };
struct alignas(16) J { const int b; };
struct K { I c; int d; };
struct L { J e; int f; };
union M { I u; };
union N { J v; };
union O { int a; int b; };
union P { int a : 1; int b : 12; };
enum Q : int { Q1, Q2 };
enum alignas(16) R : int { R1, R2 };
struct U { [[no_unique_address]] S a1; [[no_unique_address]] S a2; [[no_unique_address]] S a3; };
struct V { [[no_unique_address]] S b1; [[no_unique_address]] T b2; [[no_unique_address]] S b3; };
struct alignas(16) A : public I {};
struct alignas(16) B {};
struct C : public B, public I {};
union D { int a : 3; int b : 9; };
struct alignas(16) E { alignas(16) int a; alignas(16) int b; };
struct alignas(16) F { int c; alignas(16) int d; };
union alignas(16) G { int a; alignas(16) short b; };
union alignas(16) H { short c; int d; };
struct A1 { int a; };
struct B1 { signed int b; };
struct alignas (16) C1 : public A1 {};
struct alignas (16) D1 : public B1 {};

static_assert (!std::is_layout_compatible_v<I, J>);
static_assert (!std::is_layout_compatible_v<K, L>);
static_assert (!std::is_layout_compatible_v<M, N>);
static_assert (!std::is_layout_compatible_v<O, P>);
static_assert (!std::is_layout_compatible_v<P, D>);
static_assert (std::is_layout_compatible_v<Q, R>);
static_assert (!std::is_layout_compatible_v<U, V>);
static_assert (!std::is_layout_compatible_v<A, I>);
static_assert (!std::is_layout_compatible_v<C, I>);
static_assert (!std::is_layout_compatible_v<E, F>);
static_assert (std::is_layout_compatible_v<G, H>);
static_assert (std::is_layout_compatible_v<C1, D1>);
