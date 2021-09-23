// P0466R5
// { dg-do compile { target c++20 } }

namespace std
{
template <class S, class M>
constexpr bool
is_pointer_interconvertible_with_class (M S::*m) noexcept
{
  return __builtin_is_pointer_interconvertible_with_class (m);
}
}

struct A { int a; };

double A::*a = nullptr;
constexpr double A::*b = nullptr;
constexpr auto c = std::is_pointer_interconvertible_with_class (a);	// { dg-error "is not usable in a constant expression" }
constexpr auto d = std::is_pointer_interconvertible_with_class (b);
