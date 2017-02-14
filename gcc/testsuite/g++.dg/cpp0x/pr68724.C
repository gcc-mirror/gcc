// PR 68724 ICE in  unificiation
// { dg-do compile { target c++11 } }

template <typename _Tp, _Tp>
struct integral_constant
{
};

integral_constant<bool, true> inst;

template <typename _Tp>
struct integral_constant<bool, __is_enum(_Tp)> // { dg-error "" }
{
};

