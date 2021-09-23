// { dg-additional-options {-std=c++2a -fmodule-header} }
// { dg-module-cmi {}

#include "pr99283-6.h"

template<typename _IteratorL, typename _IteratorR>
constexpr bool
  operator<(const reverse_iterator<_IteratorL>& __x,
	    const reverse_iterator<_IteratorR>& __y);

template<typename _Tp>
  struct numeric_limits;

class __max_size_type
{
public:
  template<typename _Tp>
  constexpr
    __max_size_type(_Tp __i) noexcept
      : _M_val(__i), _M_msb(__i < 0)
  { }

  using __rep = __UINT64_TYPE__;
private:
  __rep _M_val = 0;
  unsigned _M_msb:1 = 0;
};

class __max_diff_type
{
private:
  __max_size_type _M_rep = 0;
};
