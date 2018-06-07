// PR c++/84015
// { dg-additional-options -std=c++17 }

template <int I>
struct A { };

template <int I>
struct B
{
  template<template<auto>class T>
  B(T<I>);
};

A<42> a;
B b (a);
