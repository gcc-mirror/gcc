// PR 99285 ICE with template-template-parm
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

template<typename... _Tp> struct common_type;

template<> struct common_type<> {};

template<typename _Tp0> struct common_type<_Tp0> {};

template<typename _Tp1, typename _Tp2> struct common_type<_Tp1, _Tp2> {};
