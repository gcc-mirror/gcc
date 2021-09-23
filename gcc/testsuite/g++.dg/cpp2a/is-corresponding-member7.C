// P0466R5
// { dg-do run { target c++20 } }
// { dg-options "" }

namespace std
{
template <class S1, class S2, class M1, class M2>
constexpr bool
is_corresponding_member (M1 S1::*m1, M2 S2::*m2) noexcept
{
  return __builtin_is_corresponding_member (m1, m2);
}
}

struct A { int a; struct { int b; short c; long d; }; int : 0; int e; };
struct B { const signed int a; struct { int b; signed short c; signed long d; }; volatile int e; };
struct C { int a; union { struct { short b; long c; }; long d; short e; }; signed int f; };
struct D { int a; union { long b; short c; struct { short d; signed long e; }; }; int f; };

static_assert (std::is_corresponding_member (&A::a, &B::a));
static_assert (std::is_corresponding_member (&A::b, &B::b));
static_assert (std::is_corresponding_member (&A::c, &B::c));
static_assert (std::is_corresponding_member (&A::d, &B::d));
static_assert (!std::is_corresponding_member (&A::e, &B::e));
static_assert (!std::is_corresponding_member (&A::a, &B::b));
static_assert (!std::is_corresponding_member (&A::b, &B::a));
static_assert (std::is_corresponding_member (&C::a, &D::a));
static_assert (std::is_corresponding_member (&C::f, &D::f));
static_assert (!std::is_corresponding_member (&C::a, &D::f));
static_assert (!std::is_corresponding_member (&C::f, &D::a));

int
main ()
{
  auto t1 = &A::a;
  auto t2 = &B::a;
  auto t3 = &A::b;
  auto t4 = &B::b;
  auto t5 = &A::c;
  auto t6 = &B::c;
  auto t7 = &A::d;
  auto t8 = &B::d;
  auto t9 = &A::e;
  auto t10 = &B::e;
  if (!std::is_corresponding_member (t1, t2))
    __builtin_abort ();
  if (!std::is_corresponding_member (t3, t4))
    __builtin_abort ();
  if (!std::is_corresponding_member (t5, t6))
    __builtin_abort ();
  if (!std::is_corresponding_member (t7, t8))
    __builtin_abort ();
  if (std::is_corresponding_member (t9, t10))
    __builtin_abort ();
  if (std::is_corresponding_member (t1, t4))
    __builtin_abort ();
  if (std::is_corresponding_member (t3, t2))
    __builtin_abort ();
  auto t11 = &C::a;
  auto t12 = &D::a;
  auto t13 = &C::f;
  auto t14 = &D::f;
  if (!std::is_corresponding_member (t11, t12))
    __builtin_abort ();
  if (!std::is_corresponding_member (t13, t14))
    __builtin_abort ();
  if (std::is_corresponding_member (t11, t14))
    __builtin_abort ();
  if (std::is_corresponding_member (t13, t12))
    __builtin_abort ();
}
