// { dg-do compile }

// Origin: Wolfgang Bangerth <bangerth@dealii.org>

// PR c++/15664: Template substitution of template template parameter

template <int N> struct S { 
    template<template<typename> class A> 
    friend void foo(); 
}; 
 
template<template<typename> class A> 
void foo(); 
 
template <typename> struct X {}; 
 
int main () { 
  S<1> s; 
  foo<X>(); 
}
