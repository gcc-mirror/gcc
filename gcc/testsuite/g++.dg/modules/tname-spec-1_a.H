// { dg-module-do run }
// { dg-additional-options -fmodule-header }

template<template<typename> class _Op>
using __detected_or_t = long;

template<typename _Ptr>
struct pointer_traits
{
  template<typename _Tp>
  using __difference_type = typename _Tp::difference_type;

  using difference_type = __detected_or_t<__difference_type>;
};

