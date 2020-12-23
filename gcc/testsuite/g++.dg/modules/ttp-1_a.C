// { dg-additional-options -fmodules-ts }
export module bob;

export template<template<typename> class TPL, typename T>
struct Wrapper
{
  using type = TPL<T>;
};
