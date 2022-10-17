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
}

struct A { int a; };
struct B { const int b; };
struct C { int a; unsigned int b; int f; A c; int : 0; int d; double e; };
struct D { const int x; unsigned int y; int g; B z; int u; double w; };
struct E { int a; [[no_unique_address]] int b; };
struct F { int c; const int d; };
struct G { double a; int b; double c; };
struct H { const volatile double d; int e : 16; double f; };
struct I { const double g; int h : 15; const double i; };
struct J : public A {};
struct K {};
struct L : public K, public B {};
union U { int a; };
struct V { void foo () {}; };
struct W { int a; private: int b; public: int c; };
struct Z : public A, public B {};

static_assert (std::is_corresponding_member (&A::a, &A::a));
static_assert (std::is_corresponding_member (&A::a, &B::b));
static_assert (std::is_corresponding_member (&C::a, &D::x));
static_assert (std::is_corresponding_member (&C::b, &D::y));
static_assert (std::is_corresponding_member (&C::f, &D::g));
static_assert (std::is_corresponding_member (&C::c, &D::z));
static_assert (!std::is_corresponding_member (&C::d, &D::u));
static_assert (!std::is_corresponding_member (&C::e, &D::w));
static_assert (!std::is_corresponding_member (&C::f, &D::x));
static_assert (!std::is_corresponding_member (&C::a, &D::g));
static_assert (std::is_corresponding_member (&E::a, &F::c));
static_assert (!std::is_corresponding_member (&E::b, &F::d));
static_assert (std::is_corresponding_member (&G::a, &H::d));
static_assert (!std::is_corresponding_member (&G::c, &H::f));
static_assert (std::is_corresponding_member (&H::d, &I::g));
static_assert (!std::is_corresponding_member (&H::f, &I::i));
static_assert (std::is_corresponding_member (&J::a, &B::b));
static_assert (std::is_corresponding_member<J, B, int, const int> (&J::a, &B::b));
static_assert (std::is_corresponding_member (&J::a, &L::b));
static_assert (std::is_corresponding_member<J, L, int, const int> (&J::a, &L::b));
static_assert (std::is_corresponding_member (&L::b, &B::b));
static_assert (std::is_corresponding_member<L, B, const int, const int> (&L::b, &B::b));
static_assert (!std::is_corresponding_member (&U::a, &U::a));
static_assert (!std::is_corresponding_member (&A::a, (int A::*) nullptr));
static_assert (!std::is_corresponding_member ((int A::*) nullptr, &A::a));
static_assert (!std::is_corresponding_member ((int A::*) nullptr, (int A::*) nullptr));
static_assert (!std::is_corresponding_member (&V::foo, &V::foo));
static_assert (!std::is_corresponding_member (&W::a, &W::a));
static_assert (!std::is_corresponding_member (&W::c, &W::c));
static_assert (std::is_corresponding_member (&Z::a, &Z::b));
static_assert (!std::is_corresponding_member<Z, Z, int, const int> (&Z::a, &Z::b));
