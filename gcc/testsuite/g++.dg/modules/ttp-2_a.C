// { dg-additional-options -fmodules-ts }

export module foo;
// { dg-module-bmi foo }

template<template<typename...> class Arg1>
struct TPL1;

template<template<typename...> class Arg2>
struct TPL2;
