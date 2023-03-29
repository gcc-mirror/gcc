// PR c++/84551
// { dg-do compile { target c++17_only } }
// { dg-options "-fconcepts-ts" }

template<typename> concept bool C() { return true; }

template<template<typename T> requires C<T>() class TT> struct A {};

template<typename U> requires C<U>() struct B {};

A<B> a;
