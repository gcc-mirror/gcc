// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 31 Jul 2003 <nathan@codesourcery.com>

// PR 9447. further test cases for dependent using decl

template <typename T> struct Base;

template <typename T> struct Derived : public Base<T> {
  using Base<T>::i;
  
  Derived() { i; }
  
  int get_i() { return i.f(); }
  
};
