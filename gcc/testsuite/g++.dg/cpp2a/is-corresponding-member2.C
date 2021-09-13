// P0466R5
// { dg-do run { target c++20 } }

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

int
main ()
{
  auto t1 = &A::a;
  auto t2 = &A::a;
  if (!std::is_corresponding_member (t1, t2))
    __builtin_abort ();
  auto t3 = &A::a;
  auto t4 = &B::b;
  if (!std::is_corresponding_member (t3, t4))
    __builtin_abort ();
  auto t5 = &C::a;
  auto t6 = &D::x;
  if (!std::is_corresponding_member (t5, t6))
    __builtin_abort ();
  auto t9 = &C::b;
  auto t10 = &D::y;
  if (!std::is_corresponding_member (t9, t10))
    __builtin_abort ();
  auto t11 = &C::f;
  auto t12 = &D::g;
  if (!std::is_corresponding_member (t11, t12))
    __builtin_abort ();
  auto t13 = &C::c;
  auto t14 = &D::z;
  if (!std::is_corresponding_member (t13, t14))
    __builtin_abort ();
  auto t15 = &C::d;
  auto t16 = &D::u;
  if (std::is_corresponding_member (t15, t16))
    __builtin_abort ();
  auto t17 = &C::e;
  auto t18 = &D::w;
  if (std::is_corresponding_member (t17, t18))
    __builtin_abort ();
  auto t19 = &C::f;
  auto t20 = &D::x;
  if (std::is_corresponding_member (t19, t20))
    __builtin_abort ();
  auto t21 = &C::a;
  auto t22 = &D::g;
  if (std::is_corresponding_member (t21, t22))
    __builtin_abort ();
  auto t23 = &E::a;
  auto t24 = &F::c;
  if (!std::is_corresponding_member (t23, t24))
    __builtin_abort ();
  auto t25 = &E::b;
  auto t26 = &F::d;
  if (std::is_corresponding_member (t25, t26))
    __builtin_abort ();
  auto t27 = &G::a;
  auto t28 = &H::d;
  if (!std::is_corresponding_member (t27, t28))
    __builtin_abort ();
  auto t29 = &G::c;
  auto t30 = &H::f;
  if (std::is_corresponding_member (t29, t30))
    __builtin_abort ();
  auto t31 = &H::d;
  auto t32 = &I::g;
  if (!std::is_corresponding_member (t31, t32))
    __builtin_abort ();
  auto t33 = &H::f;
  auto t34 = &I::i;
  if (std::is_corresponding_member (t33, t34))
    __builtin_abort ();
  auto t35 = &J::a;
  auto t36 = &B::b;
  if (!std::is_corresponding_member (t35, t36))
    __builtin_abort ();
  int J::*t37 = &J::a;
  const int B::*t38 = &B::b;
  if (!std::is_corresponding_member (t37, t38))
    __builtin_abort ();
  auto t39 = &J::a;
  auto t40 = &L::b;
  if (!std::is_corresponding_member (t39, t40))
    __builtin_abort ();
  int J::*t41 = &J::a;
  const int L::*t42 = &L::b;
  if (!std::is_corresponding_member (t41, t42))
    __builtin_abort ();
  auto t43 = &L::b;
  auto t44 = &B::b;
  if (!std::is_corresponding_member (t43, t44))
    __builtin_abort ();
  const int L::*t45 = &L::b;
  const int B::*t46 = &B::b;
  if (!std::is_corresponding_member (t45, t46))
    __builtin_abort ();
  auto t47 = &U::a;
  auto t48 = &U::a;
  if (std::is_corresponding_member (t47, t48))
    __builtin_abort ();
  auto t49 = &A::a;
  auto t50 = (int A::*) nullptr;
  if (std::is_corresponding_member (t49, t50))
    __builtin_abort ();
  auto t51 = (int A::*) nullptr;
  auto t52 = &A::a;
  if (std::is_corresponding_member (t51, t52))
    __builtin_abort ();
  auto t53 = (int A::*) nullptr;
  auto t54 = (int A::*) nullptr;
  if (std::is_corresponding_member (t53, t54))
    __builtin_abort ();
  auto t55 = &V::foo;
  auto t56 = &V::foo;
  if (std::is_corresponding_member (t55, t56))
    __builtin_abort ();
  auto t57 = &W::a;
  auto t58 = &W::a;
  if (std::is_corresponding_member (t57, t58))
    __builtin_abort ();
  auto t59 = &W::c;
  auto t60 = &W::c;
  if (std::is_corresponding_member (t59, t60))
    __builtin_abort ();
  auto t61 = &Z::a;
  auto t62 = &Z::b;
  if (!std::is_corresponding_member (t61, t62))
    __builtin_abort ();
  int Z::*t63 = &Z::a;
  const int Z::*t64 = &Z::b;
  if (std::is_corresponding_member (t63, t64))
    __builtin_abort ();
}
