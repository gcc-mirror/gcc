// PR c++/84551
// { dg-do compile { target c++17 } }
// { dg-options "-g -O -fconcepts" }

template<typename> concept bool C() { return true; }

template<template<typename T> requires C<T>() class> struct A {};

template<typename> requires true struct B {};

A<B> a;
