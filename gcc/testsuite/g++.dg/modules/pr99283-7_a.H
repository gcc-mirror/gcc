// PR 99283,
// { dg-additional-options "-std=c++2a -fmodule-header" }
// { dg-module-cmi {} }

#include "pr99283-7-traits.h"

template<class _CharT>
struct char_traits;

template<typename _CharT, typename _Traits = char_traits<_CharT>>
class basic_string;

typedef basic_string<char> string;

template<typename _Tp, _Tp __v>
struct integral_constant
{
  static constexpr _Tp value = __v;
  typedef _Tp value_type;
  typedef integral_constant<_Tp, __v> type;
  constexpr operator value_type() const noexcept { return value; }
  constexpr value_type operator()() const noexcept { return value; }
};

template<typename _Tp, _Tp __v>
constexpr _Tp integral_constant<_Tp, __v>::value;

typedef integral_constant<bool, true> true_type;

typedef integral_constant<bool, false> false_type;

template<bool __v>
using __bool_constant = integral_constant<bool, __v>;

template<typename _Tp, typename _Up = _Tp&&>
_Up __declval(int);

template<typename _Tp>
_Tp __declval(long);

template<typename _Tp>
auto declval() noexcept -> decltype(__declval<_Tp>(0));

struct __do_is_nothrow_swappable_impl
{
  template<typename _Tp>
  static __bool_constant<
    noexcept(swap(declval<_Tp&>(), declval<_Tp&>()))
    > __test(int);
};



template<typename _Tp>
struct __is_nothrow_swappable_impl
  : public __do_is_nothrow_swappable_impl
{
  typedef decltype(__test<_Tp>(0)) type;
};

template<typename _Tp>
struct __is_nothrow_swappable
  : public __is_nothrow_swappable_impl<_Tp>::type
{ };

#include "pr99283-7-swap.h"

class partial_ordering
{
public:
  friend constexpr bool
    operator==(partial_ordering, partial_ordering) noexcept = default;
};

class strong_ordering
{
public:
  constexpr operator partial_ordering() const noexcept;
};

template<typename _T1, typename _T2>
struct pair
{
  constexpr void
    swap(pair& __p)
    noexcept(__is_nothrow_swappable<_T1>::value);
};

template<typename _T1, typename _T2>
inline constexpr bool
  operator==(const pair<_T1, _T2>& __x, const pair<_T1, _T2>& __y);
       
template<typename _CharT>
struct char_traits
{
  using comparison_category = strong_ordering;
};
