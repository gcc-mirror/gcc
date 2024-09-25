// PR c++/84551
// { dg-do compile { target c++17 } }
// { dg-options "-fconcepts" }

template<typename> concept C = true;

template<template<typename T> requires C<T> class TT> struct A {};

template<typename U> requires C<U> struct B {};

A<B> a;
