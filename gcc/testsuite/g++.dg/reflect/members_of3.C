// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::members_of.

#include <meta>
#include <ranges>
#include <tuple>

using namespace std::meta;

struct A {
  int i;
  template <int I> int &get () { return i; }
};

template <>
struct std::tuple_size <A>
{
  static const int value = 4;
};

template <size_t I>
struct std::tuple_element <I, A>
{
  using type = int;
};

int arr[4];

namespace N1
{
  auto [x, y, z, w] = arr;
}

namespace N2
{
  auto [x, y, z, w] = A {};
}

static constexpr auto ctx = access_context::current ();
static_assert (members_of (^^N1, ctx).size () == 1);
static_assert (is_variable (members_of (^^N1, ctx)[0]));
static_assert (!has_identifier (members_of (^^N1, ctx)[0]));
static_assert (type_of (members_of (^^N1, ctx)[0]) == ^^int [4]);
static_assert (!is_structured_binding (members_of (^^N1, ctx)[0]));
static_assert (members_of (^^N2, ctx).size () == 5);
static_assert ((members_of (^^N2, ctx) | std::views::filter (is_variable) | std::ranges::to <std::vector> ()).size () == 5);
static_assert ((members_of (^^N2, ctx) | std::views::filter (has_identifier) | std::ranges::to <std::vector> ()).size () == 4);
static_assert ((members_of (^^N2, ctx) | std::views::filter (is_structured_binding) | std::ranges::to <std::vector> ()).size () == 0);
