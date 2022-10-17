// P0466R5
// { dg-do compile { target c++20 } }

namespace std
{
template <class S1, class S2, class M1, class M2>
constexpr bool
is_corresponding_member (M1 S1::*m1, M2 S2::*m2) noexcept
{
  return __builtin_is_corresponding_member (m1, m2);	// { dg-error "invalid use of incomplete type 'struct B'" }
}
}

struct A { int a; };
struct B;
constexpr int B::*n = nullptr;
constexpr auto a = std::is_corresponding_member (&A::a, n);	// { dg-error "invalid use of incomplete type 'struct B'" }
constexpr auto b = std::is_corresponding_member (n, &A::a);	// { dg-error "invalid use of incomplete type 'struct B'" }

void
foo (int B::*m)
{
  std::is_corresponding_member (&A::a, m);
  std::is_corresponding_member (m, &A::a);
}
