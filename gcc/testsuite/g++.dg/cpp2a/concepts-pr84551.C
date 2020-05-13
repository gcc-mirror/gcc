// PR c++/84551
// { dg-do compile { target c++20 } }
// { dg-additional-options "-g -O" }

template<typename> concept C = true;

template<template<typename T> requires C<T> class TT> struct A {};

template<typename U> requires true struct B {};

A<B> a;				// { dg-error "" }
