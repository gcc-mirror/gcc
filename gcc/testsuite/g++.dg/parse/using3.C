// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 6 Sep 2003 <nathan@codesourcery.com>
// Origin: stefaandr@hotmail.com

// PR c++/11794. Using decl in nested classes of a template class

template <typename T> struct a
{
  struct a1: T
  {
    using T::aa;
    
    a1() { aa = 5; }
  };
};
struct b { int aa; };
template <> struct a<int>::a1 { a1 () {} };

a<b>::a1 a_b;
a<int>::a1 a_i;
