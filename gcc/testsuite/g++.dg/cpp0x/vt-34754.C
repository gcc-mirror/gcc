// { dg-options "-std=c++0x" }
template<template<int> class... T> struct A
{
  void foo(T<0>); // { dg-error "not expanded|T" }
  void bar(T<0>); // { dg-error "not expanded|T" }
};
