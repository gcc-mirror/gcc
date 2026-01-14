// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::members_of.

#include <meta>
#include <ranges>
#include <tuple>

using namespace std::meta;

auto l1 = [] (int x) {};
auto l2 = [] (auto x) {};
auto l3 = [] (int x) static {};
auto l4 = [] (auto x) static {};

static constexpr auto ctx = access_context::current ();
// FIXME these two should pass, call operator is guaranteed
// static_assert (std::ranges::distance (members_of (type_of (^^l1), ctx) | std::views::filter (is_operator_function)) >= 1);
static_assert (std::ranges::distance (members_of (type_of (^^l2), ctx) | std::views::filter (is_operator_function_template)) >= 1);
// static_assert (std::ranges::distance (members_of (type_of (^^l3), ctx) | std::views::filter (is_operator_function)) >= 1);
static_assert (std::ranges::distance (members_of (type_of (^^l4), ctx) | std::views::filter (is_operator_function_template)) >= 1);
