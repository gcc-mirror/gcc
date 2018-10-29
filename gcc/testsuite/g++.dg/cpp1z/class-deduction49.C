// PR c++/84015
// { dg-do compile { target c++17 } }

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
