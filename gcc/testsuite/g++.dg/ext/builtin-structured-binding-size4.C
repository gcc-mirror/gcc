// { dg-do compile { target c++20 } }

namespace std {
  template <typename T> struct tuple_size;
  template <int, typename> struct tuple_element;
}

struct A { int a, b, c, d, e; };
struct B {};
struct C { int a, b; };
struct D { int a; };
typedef float V [[gnu::vector_size (16 * sizeof (float))]];
template <>
struct std::tuple_size <C> { static constexpr int value = 42; };
template <>
struct std::tuple_size <D> { static constexpr int value = 0; };

template <typename T>
concept is_destructurable = requires { { __builtin_structured_binding_size (T) }; };

static_assert (is_destructurable <A>);
static_assert (is_destructurable <const B>);
static_assert (is_destructurable <C>);
static_assert (!is_destructurable <A &>);
static_assert (!is_destructurable <int[]>);
static_assert (is_destructurable <int[1]>);
static_assert (is_destructurable <A[42]>);
static_assert (is_destructurable <float[10]>);
static_assert (!is_destructurable <int *>);
static_assert (is_destructurable <D volatile>);
static_assert (is_destructurable <const D>);
static_assert (!is_destructurable <C &&>);
