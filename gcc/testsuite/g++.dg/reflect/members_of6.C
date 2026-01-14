// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::members_of.

#include <compare>
#include <meta>
#include <ranges>

using namespace std::meta;

struct S
{
  auto operator <=> (const S &) const = default;
};

struct T
{
  auto operator <=> (this T, T) = default;
  auto operator <=> (this const T &, const T &) = default;
};

constexpr access_context uctx = access_context::unchecked ();

static_assert (members_of (^^S, uctx).size () == 8);
static_assert (operator_of (members_of (^^T, uctx)[0]) == op_spaceship);
static_assert ((members_of (^^S, uctx) | std::views::drop (1) | std::views::filter (is_special_member_function) | std::views::filter (is_defaulted) | std::ranges::to <std::vector> ()).size () == 6);
static_assert ((members_of (^^S, uctx) | std::views::drop (1) | std::views::filter (is_operator_function) | std::views::filter (is_defaulted) | std::ranges::to <std::vector> ()).size () == 3);
// TODO: Shouldn't the above have 2 implicitly-declared equality
// operators?
static_assert (members_of (^^T, uctx).size () == 9);
static_assert (operator_of (members_of (^^T, uctx)[0]) == op_spaceship);
static_assert (operator_of (members_of (^^T, uctx)[1]) == op_spaceship);
static_assert ((members_of (^^T, uctx) | std::views::drop (2) | std::views::filter (is_special_member_function) | std::views::filter (is_defaulted) | std::ranges::to <std::vector> ()).size () == 6);
static_assert ((members_of (^^T, uctx) | std::views::drop (2) | std::views::filter (is_operator_function) | std::views::filter (is_defaulted) | std::ranges::to <std::vector> ()).size () == 3);
