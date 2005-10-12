// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 2 Aug 2003 <nathan@codesourcery.com>

// PR 11704. ICE

struct A 
{
  int foo() 
  {
    return 5;
  }
};

template <class T> // If B is not template it works
struct B
{
  bool bar(A& a)
  {
    return a.foo == 0; // { dg-error "" "" }
  }
};
