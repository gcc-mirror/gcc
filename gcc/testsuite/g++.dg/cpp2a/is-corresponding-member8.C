// P0466R5
// { dg-do compile { target c++20 } }
// { dg-options "" }

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

struct A { int a; struct { short b; short c; long d; }; int : 0; int e; };
struct B { const signed int a; struct alignas(16) { short b; signed short c; signed long d; }; volatile int e; };
struct C { int a; union { struct { int b; long c; }; long d; short e; }; signed int f; };
struct D { int a; union { long b; short c; struct { int d; signed long e; }; }; int f; };

static_assert (std::is_corresponding_member (&A::a, &B::a));
static_assert (!std::is_corresponding_member (&A::b, &B::b));
static_assert (!std::is_corresponding_member (&A::c, &B::c));
static_assert (!std::is_corresponding_member (&A::d, &B::d));
auto a = std::is_corresponding_member (&C::a, &D::a);		// { dg-message "required from here" }
