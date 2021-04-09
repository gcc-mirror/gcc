// PR 99496 different types with same canonical
// (requires spec hasher to be a constant, so we get collisions)
// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

template<typename...> using __void_t = void;

template<typename _Tp, typename = void>
struct __is_referenceable
{ };

template<typename _Tp>
struct __is_referenceable<_Tp, __void_t<_Tp&>>
{ };

template<typename _Tp, bool = __is_referenceable<_Tp>::value>
struct __is_copy_constructible_impl;
