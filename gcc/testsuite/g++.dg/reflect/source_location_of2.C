// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::source_location_of.

#include <meta>

using namespace std::meta;

template <typename F>
consteval info
select_mem (info clazz, F f)
{
  for (info x : members_of (clazz, access_context::unchecked ()))
    if (f (x))
      return x;
}

consteval auto is_operator (operators op)
{
  return [op] (info mem) { return is_operator_function (mem) && operator_of (mem) == op; };
}

constexpr auto implicitDefLine = std::source_location::current ().line ()+2;
struct
ImplicitDef
{};

static_assert (source_location_of (select_mem (^^ImplicitDef, is_default_constructor)).line () == implicitDefLine);
static_assert (source_location_of (select_mem (^^ImplicitDef, is_copy_constructor)).line () == implicitDefLine);
static_assert (source_location_of (select_mem (^^ImplicitDef, is_move_constructor)).line () == implicitDefLine);
static_assert (source_location_of (select_mem (^^ImplicitDef, is_copy_assignment)).line () == implicitDefLine);
static_assert (source_location_of (select_mem (^^ImplicitDef, is_move_assignment)).line () == implicitDefLine);
static_assert (source_location_of (select_mem (^^ImplicitDef, is_destructor)).line () == implicitDefLine);

constexpr auto implicitEqLine = std::source_location::current ().line ()+3;
struct ImplicitEq
{
  auto operator<=> (const ImplicitEq&) const = default;
};

// Would expect either class head, or operator<=> line
static_assert (source_location_of (^^ImplicitEq::operator==).line () == 0);
static_assert (source_location_of (select_mem (^^ImplicitEq, is_operator (op_equals_equals))).line () == implicitEqLine);
static_assert (source_location_of (select_mem (^^ImplicitEq, is_operator (op_spaceship))).line () == implicitEqLine);
