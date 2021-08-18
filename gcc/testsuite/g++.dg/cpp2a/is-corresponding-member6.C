// P0466R5
// { dg-do compile { target c++20 } }

namespace std
{
template <class S1, class S2, class M1, class M2>
constexpr bool
is_corresponding_member (M1 S1::*m1, M2 S2::*m2) noexcept
{
  return __builtin_is_corresponding_member (m1, m2);
}
// { dg-message "'__builtin_is_corresponding_member' not well defined for anonymous unions" "" { target *-*-* } .-2 }
}

struct S {};
struct T {};
struct I { int a; };
struct alignas(16) J { const int b; };
struct K { char b; char s[15]; alignas(16) I c; short d; };
struct L { char d; char t[15]; J e; short f; };
struct U { int a0; [[no_unique_address]] S a1; [[no_unique_address]] S a2; [[no_unique_address]] S a3; short a4; };
struct V { int b0; [[no_unique_address]] S b1; [[no_unique_address]] T b2; [[no_unique_address]] S b3; short b4; };
struct U1 { int a0; [[no_unique_address]] S a1; [[no_unique_address]] S a2; [[no_unique_address]] S a3; short a4; };
struct V1 { int b0; [[no_unique_address]] S b1; [[no_unique_address]] T b2; [[no_unique_address]] S b3; short b4; };
struct A { int a; union { short b; long c; }; int d; signed char e; int f; };
struct B { const int a; union { signed long b; short c; }; volatile int d; unsigned char e; int f; };

static_assert (!std::is_corresponding_member (&K::d, &L::f));
static_assert (std::is_corresponding_member (&U::a1, &V::b1));
static_assert (!std::is_corresponding_member (&U::a2, &V::b2));
static_assert (!std::is_corresponding_member (&U::a3, &V::b3));
static_assert (!std::is_corresponding_member (&U1::a3, &V1::b3));
static_assert (!std::is_corresponding_member (&A::b, &B::c));
constexpr auto a = std::is_corresponding_member (&A::c, &B::b);		// { dg-message "required from here" }
