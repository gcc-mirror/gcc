// P0466R5
// { dg-do compile { target c++20 } }
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

static_assert (std::is_pointer_interconvertible_with_class (&W::a));
static_assert (!std::is_pointer_interconvertible_with_class (&W::b));
static_assert (!std::is_pointer_interconvertible_with_class (&W::c));
static_assert (std::is_pointer_interconvertible_with_class (&X::a));
static_assert (std::is_pointer_interconvertible_with_class (&X::b));
static_assert (!std::is_pointer_interconvertible_with_class (&X::c));
static_assert (std::is_pointer_interconvertible_with_class (&X::d));
static_assert (std::is_pointer_interconvertible_with_class (&Y::a));
static_assert (!std::is_pointer_interconvertible_with_class (&Y::b));
static_assert (!std::is_pointer_interconvertible_with_class (&Y::c));
static_assert (!std::is_pointer_interconvertible_with_class (&Y::z));
static_assert (std::is_pointer_interconvertible_with_class (&Y::d));
