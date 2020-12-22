// { dg-additional-options -fmodule-header }
// { dg-module-cmi {} }

template<template<typename> class _TQual>
struct basic_common_reference;

template<typename _Tp>
struct __xref;

template<typename _Tp1>
using __basic_common_ref
= typename basic_common_reference<__xref<_Tp1>::template __type>::type;
