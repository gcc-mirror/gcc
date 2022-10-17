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

struct A { int a; char b; };
struct B { const int c; volatile char d; };
struct C { int a : 1; int : 7; int : 0; int b : 2; };
struct D { int : 1; int c : 7; int : 0; int : 2; };
struct E { int f : 1; int : 7; int g : 2; };
struct F { int a; signed char b; };
union G { int a; long long b; signed char c; unsigned char d; int e; };
union H { long long f; unsigned char g; int h; int i; signed char j; };
struct I : public A {};
struct J {};
struct K : public J {};
struct L {};
struct M : public K, L { const int a; volatile char b; };
struct N {};
struct O : public N, M {};
struct P { int a; private: int b; public: int c; };
struct Q { int a; private: int b; public: int c; };
union U1 { int a; private: int b; public: int c; };
union U2 { int a; private: int b; public: int c; };
struct S {};
struct T {};
struct W;
struct X;
enum E1 : int { E11, E12 };
enum E2 : int { E21, E22 };
enum E3 : long { E31, E32 };
enum E4 { E41, E42 };
enum E5 { E51, E52 };

static_assert (std::is_layout_compatible<int, const int>::value);
static_assert (std::is_layout_compatible_v<double, volatile double>);
static_assert (std::is_layout_compatible_v<A, B>);
static_assert (std::is_layout_compatible_v<C, D>);
static_assert (!std::is_layout_compatible_v<int, unsigned int>);
static_assert (!std::is_layout_compatible_v<A, F>);
static_assert (std::is_layout_compatible_v<G, H>);
static_assert (std::is_layout_compatible_v<S, T>);
static_assert (std::is_layout_compatible_v<A[3], A[3]>);
static_assert (std::is_layout_compatible_v<A[], A[]>);
static_assert (!std::is_layout_compatible_v<S[1], T[1]>);
static_assert (std::is_layout_compatible_v<W[], W[]>);
static_assert (!std::is_layout_compatible_v<W[], X[]>);
static_assert (!std::is_layout_compatible_v<D, E>);
static_assert (std::is_layout_compatible_v<void, const void>);
static_assert (std::is_layout_compatible_v<I, const A>);
static_assert (std::is_layout_compatible_v<volatile A, const I>);
static_assert (std::is_layout_compatible_v<M, A>);
static_assert (std::is_layout_compatible_v<O, M>);
static_assert (std::is_layout_compatible_v<A, O>);
static_assert (std::is_layout_compatible_v<P, P>);
static_assert (!std::is_layout_compatible_v<P, Q>);
static_assert (std::is_layout_compatible_v<U1, U1>);
static_assert (!std::is_layout_compatible_v<U1, U2>);
static_assert (std::is_layout_compatible_v<E1, E2>);
static_assert (!std::is_layout_compatible_v<E1, E3>);
static_assert (std::is_layout_compatible_v<E4, E5>);
