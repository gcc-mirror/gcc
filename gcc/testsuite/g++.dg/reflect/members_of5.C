// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::members_of.

#include <meta>
#include <vector>

using namespace std::meta;

template<typename F>
consteval bool
has_mem(info clazz, F f)
{
  for (info x : members_of(clazz, access_context::unchecked()))
    if (f(x))
      return true;
  return false;
}

consteval auto is_operator(operators op)
{ return [op](info mem) { return is_operator_function(mem) && operator_of(mem) == op; }; }

struct ExplicitDef
{
  ExplicitDef() = default;
  ExplicitDef(const ExplicitDef&) = default;
  ExplicitDef(ExplicitDef&&) = default;

  ExplicitDef& operator=(const ExplicitDef&) = default;
  ExplicitDef& operator=(ExplicitDef&&) = default;
  
  ~ExplicitDef() = default;
};

static_assert (has_mem (^^ExplicitDef, is_default_constructor));
static_assert (has_mem (^^ExplicitDef, is_copy_constructor));
static_assert (has_mem (^^ExplicitDef, is_move_constructor));
static_assert (has_mem (^^ExplicitDef, is_copy_assignment));
static_assert (has_mem (^^ExplicitDef, is_move_assignment));
static_assert (has_mem (^^ExplicitDef, is_destructor));
static_assert (!has_mem (^^ExplicitDef, is_operator(op_equals_equals)));
static_assert (!has_mem (^^ExplicitDef, is_operator(op_spaceship)));

struct NoMoveDecl
{
  NoMoveDecl() = default;
  NoMoveDecl(const NoMoveDecl&) = default;

  NoMoveDecl& operator=(const NoMoveDecl&) = default;
  
  ~NoMoveDecl() = default;
};

static_assert (has_mem (^^NoMoveDecl, is_default_constructor));
static_assert (has_mem (^^NoMoveDecl, is_copy_constructor));
static_assert (!has_mem (^^NoMoveDecl, is_move_constructor));
static_assert (has_mem (^^NoMoveDecl, is_copy_assignment));
static_assert (!has_mem (^^NoMoveDecl, is_move_assignment));
static_assert (has_mem (^^NoMoveDecl, is_destructor));
static_assert (!has_mem (^^NoMoveDecl, is_operator(op_equals_equals)));
static_assert (!has_mem (^^NoMoveDecl, is_operator(op_spaceship)));

struct ImplicitDef
{
};

static_assert (has_mem (^^ImplicitDef, is_default_constructor));
static_assert (has_mem (^^ImplicitDef, is_copy_constructor));
static_assert (has_mem (^^ImplicitDef, is_move_constructor));
static_assert (has_mem (^^ImplicitDef, is_copy_assignment));
static_assert (has_mem (^^ImplicitDef, is_move_assignment));
static_assert (has_mem (^^ImplicitDef, is_destructor));
static_assert (!has_mem (^^ImplicitDef, is_operator(op_equals_equals)));
static_assert (!has_mem (^^ImplicitDef, is_operator(op_spaceship)));

struct ExplicitRelOps
{
  bool operator==(const ExplicitRelOps&) const = default;
  auto operator<=>(const ExplicitRelOps&) const = default;
};
ExplicitRelOps a,b;
static_assert (has_mem (^^ExplicitRelOps, is_operator(op_equals_equals)));
// FIXME, below check fails without this use
auto r = a <=> b;
static_assert (has_mem (^^ExplicitRelOps, is_operator(op_spaceship)));

struct NoEqualDecl
{
  auto operator<=>(const NoEqualDecl&) const
  { return std::strong_ordering::equal; }
};
static_assert (!has_mem (^^NoEqualDecl, is_operator(op_equals_equals)));
static_assert (has_mem (^^NoEqualDecl, is_operator(op_spaceship)));

struct ImplicitRelOps
{
  // operator== is implicitly declared
  auto operator<=>(const ImplicitRelOps&) const = default;
};
static_assert (has_mem (^^ImplicitRelOps, is_operator(op_equals_equals)));
static_assert (has_mem (^^ImplicitRelOps, is_operator(op_spaceship)));
