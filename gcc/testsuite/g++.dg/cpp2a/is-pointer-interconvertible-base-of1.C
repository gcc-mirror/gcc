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
struct is_pointer_interconvertible_base_of;

template<typename T, typename U>
struct is_pointer_interconvertible_base_of
  : public integral_constant <bool, __is_pointer_interconvertible_base_of (T, U)>
{
};

template <typename T, typename U>
inline constexpr bool is_pointer_interconvertible_base_of_v = __is_pointer_interconvertible_base_of (T, U);
}

struct A;
struct B { int b; };
struct C : virtual B { int c; };
struct D {};
struct E {};
struct F : public B, D, E {};
struct G : public D, E { int g; };
struct H {};
struct I : public G, H {};
struct J { int j1; private: int j2; };
struct K : public J {};
union U { int a; };

static_assert (std::is_pointer_interconvertible_base_of<A, A>::value);
static_assert (std::is_pointer_interconvertible_base_of_v<A, A>);
static_assert (std::is_pointer_interconvertible_base_of_v<const A, volatile A>);
static_assert (std::is_pointer_interconvertible_base_of_v<B, const B>);
static_assert (std::is_pointer_interconvertible_base_of_v<C, const volatile C>);
static_assert (!std::is_pointer_interconvertible_base_of_v<D, E>);
static_assert (!std::is_pointer_interconvertible_base_of_v<D, const B>);
static_assert (std::is_pointer_interconvertible_base_of_v<const B, F>);
static_assert (std::is_pointer_interconvertible_base_of_v<D, const F>);
static_assert (std::is_pointer_interconvertible_base_of_v<E, F>);
static_assert (std::is_pointer_interconvertible_base_of_v<D, volatile G>);
static_assert (std::is_pointer_interconvertible_base_of_v<const E, volatile G>);
static_assert (std::is_pointer_interconvertible_base_of_v<D, I>);
static_assert (std::is_pointer_interconvertible_base_of_v<const E, const I>);
static_assert (std::is_pointer_interconvertible_base_of_v<G, I>);
static_assert (std::is_pointer_interconvertible_base_of_v<H, volatile I>);
static_assert (!std::is_pointer_interconvertible_base_of_v<volatile J, const K>);
static_assert (!std::is_pointer_interconvertible_base_of_v<U, U>);
