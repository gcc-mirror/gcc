// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::members_of.

#include <meta>
#include <ranges>

using namespace std::meta;

namespace N1
{
  void foo (int);
  void foo (long);
  void foo (double);
  void foo (long);
  void foo (int);
  void foo (float);
}

namespace N2
{
  struct A {};
  int A;
  int B;
  struct B {};
}

namespace N3
{
  struct A {};
  void A (int);
  void A (long);
  void B (int);
  void B (long);
  struct B {};
}

namespace N4
{
  static union { int a; };
}

static constexpr auto ctx = access_context::current ();
static_assert (members_of (^^N1, ctx).size () == 4);
static_assert ((members_of (^^N1, ctx) | std::views::filter (is_function) | std::ranges::to <std::vector> ()).size () == 4);
static_assert ((members_of (^^N1, ctx) | std::views::filter (has_identifier) | std::ranges::to <std::vector> ()).size () == 4);
static_assert (members_of (^^N2, ctx).size () == 4);
static_assert ((members_of (^^N2, ctx) | std::views::filter (is_variable) | std::ranges::to <std::vector> ()).size () == 2);
static_assert ((members_of (^^N2, ctx) | std::views::filter (is_type) | std::ranges::to <std::vector> ()).size () == 2);
static_assert ((members_of (^^N2, ctx) | std::views::filter (has_identifier) | std::ranges::to <std::vector> ()).size () == 4);
static_assert (members_of (^^N3, ctx).size () == 6);
static_assert ((members_of (^^N3, ctx) | std::views::filter (is_function) | std::ranges::to <std::vector> ()).size () == 4);
static_assert ((members_of (^^N3, ctx) | std::views::filter (is_type) | std::ranges::to <std::vector> ()).size () == 2);
static_assert ((members_of (^^N3, ctx) | std::views::filter (has_identifier) | std::ranges::to <std::vector> ()).size () == 6);
static_assert (members_of (^^N4, ctx).size () == 2);
static_assert ((members_of (^^N4, ctx) | std::views::filter (is_variable) | std::ranges::to <std::vector> ()).size () == 1);
static_assert ((members_of (^^N4, ctx) | std::views::filter (is_type) | std::views::filter (is_union_type) | std::ranges::to <std::vector> ()).size () == 1);
static_assert ((members_of (^^N4, ctx) | std::views::filter (has_identifier) | std::ranges::to <std::vector> ()).size () == 0);
