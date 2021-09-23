// P0466R5
// { dg-do compile { target c++20 } }

namespace std
{
template <typename T, T v>
struct integral_constant
{
  static constexpr T value = v;
};

template <typename, typename>
struct is_layout_compatible;

template<typename T, typename U>
struct is_layout_compatible
  : public integral_constant <bool, __is_layout_compatible (T, U)>
{
};

template <typename T, typename U>
inline constexpr bool is_layout_compatible_v = __is_layout_compatible (T, U);
}
// { dg-error "invalid use of incomplete type 'struct W'" "" { target *-*-* } .-2 }
// { dg-error "invalid use of incomplete type 'struct \[XY]'" "" { target *-*-* } .-3 }
// { dg-error "invalid use of incomplete type 'struct Z'" "" { target *-*-* } .-4 }

struct W;
struct X;
struct Y;
struct Z;
struct A {};

auto a = std::is_layout_compatible_v<W, W>;
auto b = std::is_layout_compatible_v<X, Y>;
auto c = std::is_layout_compatible_v<A, Z>;
