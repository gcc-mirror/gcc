// { dg-additional-options -fmodules-ts }

export module foo;
// { dg-module-cmi foo }

template<typename Arg1, template<typename> typename Arg2>
struct TPL
{
  using type = char;
};

/// Implementation of the detection idiom (positive case).
template<template<typename> typename Op>
struct TPL <Op<int>, Op>
{
  using type = int;
};
