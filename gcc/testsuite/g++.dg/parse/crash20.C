// { dg-do compile }

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 1 Dec 2004 <nathan@codesourcery.com>

// PR 18729: ICE on ill formed
// Origin: Volker Reichelt <reichelt@gcc.gnu.org>

template<typename T> struct A
{
  typedef typename T::X Y; // { dg-error "not a class" "" }
};

A<int>::Y y; // { dg-error "instantiated from here" "" }
