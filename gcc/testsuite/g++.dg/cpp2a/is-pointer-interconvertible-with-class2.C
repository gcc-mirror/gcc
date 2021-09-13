// P0466R5
// { dg-do run { target c++20 } }

namespace std
{
template <class S, class M>
constexpr bool
is_pointer_interconvertible_with_class (M S::*m) noexcept
{
  return __builtin_is_pointer_interconvertible_with_class (m);
}
}

struct A;
struct B { int b; double b2; };
struct C : virtual B { int c; };
struct D {};
struct E {};
struct F : public B, D, E {};
struct G : public D, E { int g; };
struct H {};
struct I : public G, H {};
struct J { int j1; private: int j2; public: int j3; };
struct K : public J {};
struct L : public B, D, E {};
struct M { D d [[no_unique_address]]; E e [[no_unique_address]]; int f; };
union U { int a; double b; long long c; };
struct V { union { int a; long b; }; int c; };
union X { int a; union { short b; long c; }; long long d; };
struct Y { void foo () {} };
union Z { int a; private: int b; protected: int c; public: int d; };

int
main ()
{
  auto t1 = &B::b;
  if (!std::is_pointer_interconvertible_with_class (t1))
    __builtin_abort ();
  auto t2 = &B::b2;
  if (std::is_pointer_interconvertible_with_class (t2))
    __builtin_abort ();
  auto t3 = &C::b;
  if (!std::is_pointer_interconvertible_with_class (t3))
    __builtin_abort ();
  auto t4 = &F::b;
  if (!std::is_pointer_interconvertible_with_class (t4))
    __builtin_abort ();
  int F::*t5 = &F::b;
  if (!std::is_pointer_interconvertible_with_class (t5))
    __builtin_abort ();
  auto t6 = &G::g;
  if (!std::is_pointer_interconvertible_with_class (t6))
    __builtin_abort ();
  int G::*t7 = &G::g;
  if (!std::is_pointer_interconvertible_with_class (t7))
    __builtin_abort ();
  auto t8 = &I::g;
  if (!std::is_pointer_interconvertible_with_class (t8))
    __builtin_abort ();
  int I::*t9 = &I::g;
  if (!std::is_pointer_interconvertible_with_class (t9))
    __builtin_abort ();
  auto t10 = &J::j1;
  if (std::is_pointer_interconvertible_with_class (t10))
    __builtin_abort ();
  auto t11 = &J::j3;
  if (std::is_pointer_interconvertible_with_class (t11))
    __builtin_abort ();
  auto t12 = &K::j1;
  if (std::is_pointer_interconvertible_with_class (t12))
    __builtin_abort ();
  auto t13 = &K::j3;
  if (std::is_pointer_interconvertible_with_class (t13))
    __builtin_abort ();
  auto t14 = &L::b;
  if (!std::is_pointer_interconvertible_with_class (t14))
    __builtin_abort ();
  int L::*t15 = &L::b;
  if (!std::is_pointer_interconvertible_with_class (t15))
    __builtin_abort ();
  auto t16 = &L::b;
  if (!std::is_pointer_interconvertible_with_class (t16))
    __builtin_abort ();
  auto t17 = &M::d;
  if (!std::is_pointer_interconvertible_with_class (t17))
    __builtin_abort ();
  auto t18 = &M::e;
  if (std::is_pointer_interconvertible_with_class (t18))
    __builtin_abort ();
  auto t19 = &M::f;
  if (std::is_pointer_interconvertible_with_class (t19))
    __builtin_abort ();
  auto t20 = &U::a;
  if (!std::is_pointer_interconvertible_with_class (t20))
    __builtin_abort ();
  auto t21 = &U::b;
  if (!std::is_pointer_interconvertible_with_class (t21))
    __builtin_abort ();
  auto t22 = &U::c;
  if (!std::is_pointer_interconvertible_with_class (t22))
    __builtin_abort ();
  auto t23 = &V::a;
  if (!std::is_pointer_interconvertible_with_class (t23))
    __builtin_abort ();
  auto t24 = &V::b;
  if (!std::is_pointer_interconvertible_with_class (t24))
    __builtin_abort ();
  auto t25 = &V::c;
  if (std::is_pointer_interconvertible_with_class (t25))
    __builtin_abort ();
  auto t26 = &X::a;
  if (!std::is_pointer_interconvertible_with_class (t26))
    __builtin_abort ();
  auto t27 = &X::b;
  if (!std::is_pointer_interconvertible_with_class (t27))
    __builtin_abort ();
  auto t28 = &X::c;
  if (!std::is_pointer_interconvertible_with_class (t28))
    __builtin_abort ();
  auto t29 = &X::d;
  if (!std::is_pointer_interconvertible_with_class (t29))
    __builtin_abort ();
  auto t30 = (int B::*) nullptr;
  if (std::is_pointer_interconvertible_with_class (t30))
    __builtin_abort ();
  auto t31 = &Y::foo;
  if (std::is_pointer_interconvertible_with_class (t31))
    __builtin_abort ();
  auto t32 = &Z::a;
  if (!std::is_pointer_interconvertible_with_class (t32))
    __builtin_abort ();
  auto t33 = &Z::d;
  if (!std::is_pointer_interconvertible_with_class (t33))
    __builtin_abort ();
}
