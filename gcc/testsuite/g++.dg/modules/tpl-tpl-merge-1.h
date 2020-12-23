typedef long unsigned int size_t;

template<typename _Tp, typename _Up>
struct Replace;

template<template<typename> class _Template>
struct Replace<_Template<char>, char>
{
  using type = _Template<char>;
};

template<typename _Tp>
struct TPL;

template<typename _Alloc>
struct Traits
{
  template<typename _Tp>
  using Rebind = typename Replace<_Alloc, _Tp>::type;
};

using tdef = Traits<TPL<char>>::template Rebind<char>;
