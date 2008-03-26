// { dg-options "-std=c++0x" }

template<template<typename...> class TT>
TT<int, float, double> foo(TT<int, float>)
{
  return TT<int, float, double>();
}

template<typename T>
int& foo(T) 
{ 
  static int i = 0; return i; 
}

template<typename T, typename U>
struct pair {};

void bar()
{
  pair<int, float> p;
  int& i = foo(p);
}

