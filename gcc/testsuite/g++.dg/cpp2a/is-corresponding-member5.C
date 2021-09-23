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

struct S {};
struct T {};
struct I { int a; };
struct alignas(16) J { const int b; };
struct K { char b; char s[15]; I c; short d; };
struct L { char d; char t[15]; J e; short f; };
struct U { int a0; [[no_unique_address]] S a1; [[no_unique_address]] S a2; [[no_unique_address]] S a3; short a4; };
struct V { int b0; [[no_unique_address]] S b1; [[no_unique_address]] T b2; [[no_unique_address]] S b3; short b4; };
struct U1 { int a0; [[no_unique_address]] S a1; [[no_unique_address]] S a2; [[no_unique_address]] S a3; short a4; };
struct V1 { int b0; [[no_unique_address]] S b1; [[no_unique_address]] T b2; [[no_unique_address]] S b3; short b4; };
struct A { int a; union { short b; long c; }; int d; signed char e; int f; };
struct B { const int a; union { signed long b; short c; }; volatile int d; unsigned char e; int f; };
struct A1 { int a; union { short b; long c; }; int d; short e; int f; };
struct B1 { const int a; union { signed long b; short c; }; volatile int d; unsigned short e; int f; };

static_assert (std::is_corresponding_member (&I::a, &J::b));
static_assert (std::is_corresponding_member (&K::b, &L::d));
static_assert (!std::is_corresponding_member (&K::c, &L::e));
static_assert (std::is_corresponding_member (&U::a0, &V::b0));
static_assert (!std::is_corresponding_member (&U::a4, &V::b4));
static_assert (std::is_corresponding_member (&A::a, &B::a));
static_assert (std::is_corresponding_member (&A::d, &B::d));
static_assert (!std::is_corresponding_member (&A::e, &B::e));
static_assert (!std::is_corresponding_member (&A::f, &B::f));
static_assert (!std::is_corresponding_member (&A::a, &B::f));
static_assert (!std::is_corresponding_member (&A::d, &B::a));
static_assert (!std::is_corresponding_member (&A::a, &B::d));
static_assert (!std::is_corresponding_member (&A::f, &B::a));
static_assert (!std::is_corresponding_member (&A1::e, &B1::e));

int
main ()
{
  auto t1 = &I::a;
  auto t2 = &J::b;
  if (!std::is_corresponding_member (t1, t2))
    __builtin_abort ();
  auto t3 = &K::b;
  auto t4 = &L::d;
  if (!std::is_corresponding_member (t3, t4))
    __builtin_abort ();
  auto t5 = &K::c;
  auto t6 = &L::e;
  if (std::is_corresponding_member (t5, t6))
    __builtin_abort ();
  auto t7 = &U::a0;
  auto t8 = &V::b0;
  if (!std::is_corresponding_member (t7, t8))
    __builtin_abort ();
  auto t9 = &U::a4;
  auto t10 = &V::b4;
  if (std::is_corresponding_member (t9, t10))
    __builtin_abort ();
  auto t11 = &A::a;
  auto t12 = &B::a;
  auto t13 = &A::d;
  auto t14 = &B::d;
  auto t15 = &A::e;
  auto t16 = &B::e;
  auto t17 = &A::f;
  auto t18 = &B::f;
  if (!std::is_corresponding_member (t11, t12))
    __builtin_abort ();
  if (!std::is_corresponding_member (t13, t14))
    __builtin_abort ();
  if (std::is_corresponding_member (t15, t16))
    __builtin_abort ();
  if (std::is_corresponding_member (t17, t18))
    __builtin_abort ();
  if (std::is_corresponding_member (t11, t18))
    __builtin_abort ();
  if (std::is_corresponding_member (t13, t12))
    __builtin_abort ();
  if (std::is_corresponding_member (t11, t14))
    __builtin_abort ();
  if (std::is_corresponding_member (t17, t12))
    __builtin_abort ();
  auto t19 = &A1::e;
  auto t20 = &B1::e;
  if (std::is_corresponding_member (t19, t20))
    __builtin_abort ();
}
