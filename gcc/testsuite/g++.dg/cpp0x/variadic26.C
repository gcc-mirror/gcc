// { dg-options "-std=gnu++11" }
template<template<int, int> class Meta, int Initial, int... Values>
struct accumulate {
  static const int value = Initial;
};

template<template<int, int> class Meta, int Initial, int Value, int... Rest>
struct accumulate<Meta, Initial, Value, Rest...> {
  static const int value = 
    Meta<Value, accumulate<Meta, Initial, Rest...>::value>::value;
};

template<int X, int Y>
struct sum {
  static const int value = X + Y;
};

template<int X, int Y>
struct prod {
  static const int value = X * Y;
};

int a0[accumulate<sum,0,1,2,3,4,5>::value == 15? 1 : -1];
int a1[accumulate<prod,1,1,2,3,4,5>::value == 120? 1 : -1];
