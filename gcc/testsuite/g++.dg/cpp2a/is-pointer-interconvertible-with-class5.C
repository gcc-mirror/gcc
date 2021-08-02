// P0466R5
// { dg-do run { target c++20 } }
// { dg-options "" }

namespace std
{
template <class S, class M>
constexpr bool
is_pointer_interconvertible_with_class (M S::*m) noexcept
{
  return __builtin_is_pointer_interconvertible_with_class (m);
}
}

struct W { struct { int a; long b; }; int c; };
union X { int a; struct { short b; long c; }; long long d; };
struct D { int x; private: int y; };                                                                                                                                                  
union Y { int a; struct { short b; long c; D z; }; long long d; };                                                                                                                    

int
main ()
{
  auto t1 = &W::a;
  if (!std::is_pointer_interconvertible_with_class (t1))
    __builtin_abort ();
  auto t2 = &W::b;
  if (std::is_pointer_interconvertible_with_class (t2))
    __builtin_abort ();
  auto t3 = &W::c;
  if (std::is_pointer_interconvertible_with_class (t3))
    __builtin_abort ();
  auto t4 = &X::a;
  if (!std::is_pointer_interconvertible_with_class (t4))
    __builtin_abort ();
  auto t5 = &X::b;
  if (!std::is_pointer_interconvertible_with_class (t5))
    __builtin_abort ();
  auto t6 = &X::c;
  if (std::is_pointer_interconvertible_with_class (t6))
    __builtin_abort ();
  auto t7 = &X::d;
  if (!std::is_pointer_interconvertible_with_class (t7))
    __builtin_abort ();
  auto t8 = &Y::a;
  if (!std::is_pointer_interconvertible_with_class (t8))
    __builtin_abort ();
  auto t9 = &Y::b;
  if (std::is_pointer_interconvertible_with_class (t9))
    __builtin_abort ();
  auto t10 = &Y::c;
  if (std::is_pointer_interconvertible_with_class (t10))
    __builtin_abort ();
  auto t11 = &Y::z;
  if (std::is_pointer_interconvertible_with_class (t11))
    __builtin_abort ();
  auto t12 = &Y::d;
  if (!std::is_pointer_interconvertible_with_class (t12))
    __builtin_abort ();
}
