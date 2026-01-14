// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_deleted.

#include <meta>
#include <vector>

using namespace std::meta;

template<typename F>
consteval info
select_mem(info clazz, F f)
{
  for (info x : members_of(clazz, access_context::unchecked()))
    if (f(x))
      return x;
}

struct ExplicitDef
{
  ExplicitDef() = default;
  ExplicitDef(const ExplicitDef&) = default;
  ExplicitDef(ExplicitDef&&) = default;

  ExplicitDef& operator=(const ExplicitDef&) = default;
  ExplicitDef& operator=(ExplicitDef&&) = default;
  
  ~ExplicitDef() = default;
};

static_assert (is_defaulted (select_mem (^^ExplicitDef, is_default_constructor)));
static_assert (is_defaulted (select_mem (^^ExplicitDef, is_copy_constructor)));
static_assert (is_defaulted (select_mem (^^ExplicitDef, is_move_constructor)));
static_assert (is_defaulted (select_mem (^^ExplicitDef, is_copy_assignment)));
static_assert (is_defaulted (select_mem (^^ExplicitDef, is_move_assignment)));
static_assert (is_defaulted (select_mem (^^ExplicitDef, is_destructor)));

struct ImplicitDef
{
};

static_assert (is_defaulted (select_mem (^^ImplicitDef, is_default_constructor)));
static_assert (is_defaulted (select_mem (^^ImplicitDef, is_copy_constructor)));
static_assert (is_defaulted (select_mem (^^ImplicitDef, is_move_constructor)));
static_assert (is_defaulted (select_mem (^^ImplicitDef, is_copy_assignment)));
static_assert (is_defaulted (select_mem (^^ImplicitDef, is_move_assignment)));
static_assert (is_defaulted (select_mem (^^ImplicitDef, is_destructor)));

struct ExplicitRelOps
{
  bool operator==(const ExplicitRelOps&) const = default;
  auto operator<=>(const ExplicitRelOps&) const = default;
};
static_assert (is_defaulted (^^ExplicitRelOps::operator==));
static_assert (is_defaulted (^^ExplicitRelOps::operator<=>));

struct ImplicitRelOps
{
  // operator== is implicitly declared
  auto operator<=>(const ImplicitRelOps&) const = default;
};
static_assert (is_defaulted (^^ImplicitRelOps::operator==));
static_assert (is_defaulted (^^ImplicitRelOps::operator<=>));

