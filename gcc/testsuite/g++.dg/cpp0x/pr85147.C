// PR c++/85147
// { dg-do compile { target c++11 } }

template<typename T> struct A
{
  template<template<...T> class...> struct B {};	// { dg-error "expected|mismatch" }
};

A<int>::B<> b;						// { dg-error "does not name a template type" }
