template<typename _Tp>
struct __is_integer
{
  enum { __value = 0 };
};

template<typename _Tp>
struct __is_integer_nonstrict

{
  using __is_integer<_Tp>::__value;

  enum { __width = __value ? sizeof(_Tp) * 8 : 0 };
};
