// { dg-additional-options -fmodules-ts }

export module foo;
// { dg-module-cmi foo }

template<typename _Tp, _Tp __v>
struct integral_constant
{};

template<bool __v>
using __bool_constant = integral_constant<bool, __v>;

template<typename _Tp, typename... _Args>
struct __is_constructible_impl
  : public __bool_constant<__is_constructible(_Tp, _Args...)>
{ };

