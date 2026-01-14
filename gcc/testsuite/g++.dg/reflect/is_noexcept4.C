// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::is_noexcept.

#include <meta>
#include <vector>

using namespace std::meta;

template<bool Noex>
struct Mem {
  Mem() noexcept(Noex);
  Mem(Mem const&) noexcept(Noex);
  Mem(Mem&&) noexcept(Noex);
  Mem& operator=(Mem const&) noexcept(Noex);
  Mem& operator=(Mem&&) noexcept(Noex);
  ~Mem() noexcept(Noex);

  bool operator==(const Mem&) const noexcept(Noex);
  std::strong_ordering operator<=>(const Mem&) const noexcept(Noex);
};

template<typename F>
consteval info
select_mem(info clazz, F f)
{
  for (info x : members_of(clazz, access_context::unchecked()))
    if (f(x))
      return x;
}

template<bool Noex>
struct ExplicitDef
{
  ExplicitDef() = default;
  ExplicitDef(const ExplicitDef&) = default;
  ExplicitDef(ExplicitDef&&) = default;

  ExplicitDef& operator=(const ExplicitDef&) = default;
  ExplicitDef& operator=(ExplicitDef&&) = default;

  ~ExplicitDef() = default;

  Mem<Noex> d;
};

static_assert (is_noexcept (select_mem (^^ExplicitDef<true>, is_default_constructor)));
static_assert (is_noexcept (select_mem (^^ExplicitDef<true>, is_copy_constructor)));
static_assert (is_noexcept (select_mem (^^ExplicitDef<true>, is_move_constructor)));
static_assert (is_noexcept (select_mem (^^ExplicitDef<true>, is_copy_assignment)));
static_assert (is_noexcept (select_mem (^^ExplicitDef<true>, is_move_assignment)));
static_assert (is_noexcept (select_mem (^^ExplicitDef<true>, is_destructor)));

static_assert (!is_noexcept (select_mem (^^ExplicitDef<false>, is_default_constructor)));
static_assert (!is_noexcept (select_mem (^^ExplicitDef<false>, is_copy_constructor)));
static_assert (!is_noexcept (select_mem (^^ExplicitDef<false>, is_move_constructor)));
static_assert (!is_noexcept (select_mem (^^ExplicitDef<false>, is_copy_assignment)));
static_assert (!is_noexcept (select_mem (^^ExplicitDef<false>, is_move_assignment)));
static_assert (!is_noexcept (select_mem (^^ExplicitDef<false>, is_destructor)));

template<bool Noex>
struct ImplicitDef
{
  Mem<Noex> d;
};

static_assert (is_noexcept (select_mem (^^ImplicitDef<true>, is_default_constructor)));
static_assert (is_noexcept (select_mem (^^ImplicitDef<true>, is_copy_constructor)));
static_assert (is_noexcept (select_mem (^^ImplicitDef<true>, is_move_constructor)));
static_assert (is_noexcept (select_mem (^^ImplicitDef<true>, is_copy_assignment)));
static_assert (is_noexcept (select_mem (^^ImplicitDef<true>, is_move_assignment)));
static_assert (is_noexcept (select_mem (^^ImplicitDef<true>, is_destructor)));

static_assert (!is_noexcept (select_mem (^^ImplicitDef<false>, is_default_constructor)));
static_assert (!is_noexcept (select_mem (^^ImplicitDef<false>, is_copy_constructor)));
static_assert (!is_noexcept (select_mem (^^ImplicitDef<false>, is_move_constructor)));
static_assert (!is_noexcept (select_mem (^^ImplicitDef<false>, is_copy_assignment)));
static_assert (!is_noexcept (select_mem (^^ImplicitDef<false>, is_move_assignment)));
static_assert (!is_noexcept (select_mem (^^ImplicitDef<false>, is_destructor)));

template<bool Noex>
struct ExplicitRelOps
{
  bool operator==(const ExplicitRelOps&) const = default;
  auto operator<=>(const ExplicitRelOps&) const = default;

  Mem<Noex> d;
};
static_assert (is_noexcept (^^ExplicitRelOps<true>::operator==));
static_assert (is_noexcept (^^ExplicitRelOps<true>::operator<=>));

ExplicitRelOps<true> x1;
static_assert (noexcept (x1 == x1));
static_assert (noexcept (x1 <=> x1));
static_assert (is_noexcept (^^ExplicitRelOps<true>::operator==));
static_assert (is_noexcept (^^ExplicitRelOps<true>::operator<=>));

static_assert (!is_noexcept (^^ExplicitRelOps<false>::operator==));
static_assert (!is_noexcept (^^ExplicitRelOps<false>::operator<=>));

template<bool Noex>
struct ImplicitRelOps
{
  // operator== is implicitly declared
  auto operator<=>(const ImplicitRelOps&) const = default;

  Mem<Noex> d;
};

static_assert (is_noexcept (^^ImplicitRelOps<true>::operator==));
static_assert (is_noexcept (^^ImplicitRelOps<true>::operator<=>));

ImplicitRelOps<true> x2;
static_assert (noexcept (x2 == x2));
static_assert (noexcept (x2 <=> x2));
static_assert (is_noexcept (^^ImplicitRelOps<true>::operator==));
static_assert (is_noexcept (^^ImplicitRelOps<true>::operator<=>));

static_assert (!is_noexcept (^^ImplicitRelOps<false>::operator==));
static_assert (!is_noexcept (^^ImplicitRelOps<false>::operator<=>));
