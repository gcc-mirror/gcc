// PR c++/32565
// { dg-do compile }
// { dg-options "-std=c++0x" }

template<typename...> struct A1;
template<template<int, int...> class T> struct A1<T<0, 1> > {};
template<int, int, int...> struct B1 {};
A1<B1<0, 1> > a1; // { dg-error "incomplete type" }
template<int...> struct B2 {};
A1<B2<0, 1> > a2; // { dg-error "incomplete type" }
