// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_deleted.

#include <meta>
#include <vector>

using namespace std::meta;

struct Del {
  Del() = delete;
  Del(Del&&) = delete;
  ~Del() = delete;
};

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

  Del d;
};

static_assert (is_defaulted (select_mem (^^ExplicitDef, is_default_constructor)));
static_assert (is_deleted (select_mem (^^ExplicitDef, is_default_constructor)));
static_assert (is_defaulted (select_mem (^^ExplicitDef, is_copy_constructor)));
static_assert (is_deleted (select_mem (^^ExplicitDef, is_copy_constructor)));
static_assert (is_defaulted (select_mem (^^ExplicitDef, is_move_constructor)));
static_assert (is_deleted (select_mem (^^ExplicitDef, is_move_constructor)));
static_assert (is_defaulted (select_mem (^^ExplicitDef, is_copy_assignment)));
static_assert (is_deleted (select_mem (^^ExplicitDef, is_copy_assignment)));
static_assert (is_defaulted (select_mem (^^ExplicitDef, is_move_assignment)));
static_assert (is_deleted (select_mem (^^ExplicitDef, is_move_assignment)));
static_assert (is_defaulted (select_mem (^^ExplicitDef, is_destructor)));
static_assert (is_deleted (select_mem (^^ExplicitDef, is_destructor)));

struct ImplicitDef
{
  Del d;
};

static_assert (is_defaulted (select_mem (^^ImplicitDef, is_default_constructor)));
static_assert (is_deleted (select_mem (^^ImplicitDef, is_default_constructor)));
static_assert (is_defaulted (select_mem (^^ImplicitDef, is_copy_constructor)));
static_assert (is_deleted (select_mem (^^ImplicitDef, is_copy_constructor)));
static_assert (is_defaulted (select_mem (^^ImplicitDef, is_move_constructor)));
static_assert (is_deleted (select_mem (^^ImplicitDef, is_move_constructor)));
static_assert (is_defaulted (select_mem (^^ImplicitDef, is_copy_assignment)));
static_assert (is_deleted (select_mem (^^ImplicitDef, is_copy_assignment)));
static_assert (is_defaulted (select_mem (^^ImplicitDef, is_move_assignment)));
static_assert (is_deleted (select_mem (^^ImplicitDef, is_move_assignment)));
static_assert (is_defaulted (select_mem (^^ImplicitDef, is_destructor)));
static_assert (is_deleted (select_mem (^^ImplicitDef, is_destructor)));

template<typename T>
concept check_rel_ops = requires(T t) {
  t <=> t;
  t == t;
};

struct RelOpsDel
{
  int& x;

  // deleted because of reference member
  auto operator<=>(const RelOpsDel&) const = default;
};
static_assert (is_deleted (^^RelOpsDel::operator<=>));
static_assert (!check_rel_ops<RelOpsDel>);

static_assert (is_defaulted (^^RelOpsDel::operator==));
static_assert (is_deleted (^^RelOpsDel::operator==));
static_assert (is_defaulted (^^RelOpsDel::operator<=>));
static_assert (is_deleted (^^RelOpsDel::operator<=>));
