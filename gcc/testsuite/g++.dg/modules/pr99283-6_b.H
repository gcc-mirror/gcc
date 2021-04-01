// { dg-additional-options {-std=c++2a -fmodule-header} }
// { dg-module-cmi {}

#include "pr99283-6.h"

template<typename _Tp>
void __addressof(_Tp& __r)  ;

template<typename _Tp, _Tp __v>
struct integral_constant
{
  static constexpr _Tp value = __v;
};

template<typename _Tp, _Tp __v>
constexpr _Tp integral_constant<_Tp, __v>::value;

typedef integral_constant<bool, true> true_type;
typedef integral_constant<bool, false> false_type;

template<typename _B1, typename _B2>
struct __and_
  : public conditional<_B1::value, _B2, _B1>::type
{ };

template<typename _From, typename _To>
struct is_convertible
  : public true_type
{ };

template<bool, typename _Tp = void>
struct enable_if
{ };

template<typename _Tp>
struct enable_if<true, _Tp>
{ typedef _Tp type; };

template<bool _Cond, typename _Tp = void>
using __enable_if_t = typename enable_if<_Cond, _Tp>::type;

template<typename A, typename B>
using _Require = __enable_if_t<__and_<A, B>::value>;

template<bool _Cond, typename _Iftrue, typename _Iffalse>
struct conditional
{ typedef _Iftrue type; };

template<typename> struct iterator_traits;


template<typename _IteratorL, typename _IteratorR>
constexpr bool
  operator!=(const reverse_iterator<_IteratorL>& __x,
	     const reverse_iterator<_IteratorR>& __y);

typedef __INT64_TYPE__ int64_t;
typedef int64_t intmax_t;

template<intmax_t _Num>
struct ratio
{
};

namespace chrono
{
template<typename _Rep>
struct duration;

template<typename _ToDur, typename _Rep>
constexpr _ToDur
  duration_cast(const duration<_Rep>& __d)
{
  typedef typename _ToDur::rep __to_rep;
  return _ToDur(static_cast<__to_rep>(static_cast<intmax_t>(__d.count())));
}

template<typename _Rep>
struct duration
{
  
public:
  using rep = _Rep;
  
  constexpr duration() = default;

  duration(const duration&) = default;

  template<typename _Rep2, typename
	   = _Require<is_convertible<const _Rep2&, rep>,
		      true_type>>
  constexpr explicit duration(const _Rep2& __rep)
    : __r (__rep) {}
  
  ~duration() = default;
  duration& operator=(const duration&) = default;

  rep count() const;
  
private:
  rep __r;
};

using seconds = duration<int64_t>;

template<typename _Clock, typename _Dur>
struct time_point
{
  typedef _Dur duration;

  duration time_since_epoch() const;

private:
  duration __d;
};

struct system_clock
{
  typedef chrono::seconds duration;
	 
  typedef chrono::time_point<system_clock, duration> time_point;

  static void
    to_time_t(const time_point& __t) noexcept
  {
    duration_cast<chrono::seconds>
     (__t.time_since_epoch()).count();
  }
};

}

template<typename>
class allocator;

template<typename _ForwardIterator>
constexpr void
  __destroy(_ForwardIterator __first, _ForwardIterator __last)
{
  for (; __first != __last; ++__first)
    __addressof (*__first);
}

template<typename _Alloc>
struct allocator_traits
{
private:
  template<typename _Alloc2, typename _Tp>
  static constexpr void
    _S_destroy(_Alloc2&, _Tp* __p, ...)
    noexcept
  {
    __destroy(__p);
  }
};

template<typename _Tp>
struct allocator_traits<allocator<_Tp>>
{
  using value_type = _Tp;
  using pointer = _Tp*;
};

import "pr99283-6_a.H";
