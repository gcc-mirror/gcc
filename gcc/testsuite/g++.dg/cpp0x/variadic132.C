// PR c++/53305
// { dg-do compile { target c++11 } }

template<class... Ts> struct tuple { };

struct funct
{
  template<class... argTs>
  int operator()(argTs...);
};

template<class...> struct test;

template<template <class...> class tp,
	 class... arg1Ts, class... arg2Ts>
struct test<tp<arg1Ts...>, tp<arg2Ts...>>
{
  template<class func, class...arg3Ts>
    auto test2(func fun, arg1Ts... arg1s, arg3Ts... arg3s)
    -> decltype(fun(arg1s..., arg3s...));
};

int main()
{
  test<tuple<>, tuple<char,int>> t2;
  t2.test2(funct(), 'a', 2);
}
