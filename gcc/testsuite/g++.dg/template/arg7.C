// PR c++/27425, 34274

template<typename T> struct A
{
  template<template<T> class> struct B {}; // { dg-error "void|mismatch|expected" }
  // { dg-bogus "not supported" "" { target *-*-* } 5 }
  template<T> struct C;			   // { dg-error "void" }
  B<C> b;
};

A<void> a;			// { dg-message "required" }
