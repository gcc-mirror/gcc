// PR c++/33965
// { dg-options -std=c++11 }
template<typename T>
struct foo
{
    static bool const value = false;
};

template<template<typename...> class T, typename... Args>
struct foo<T<Args...> >
{
    static bool const value = true;
};

template<int I>
struct int_
{};

int main()
{
  static_assert(foo<int_<0> >::value == false, 
		"picked up partial specialization, but should not have");
}
