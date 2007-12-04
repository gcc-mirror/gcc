// { dg-options -std=c++0x }
// PR c++/34101
template<typename> struct A {};

template<template<typename> class...> struct B {};

template<template<typename> class T> void foo(const B<T>&);

void bar()
{
  foo(B<A>());
}
