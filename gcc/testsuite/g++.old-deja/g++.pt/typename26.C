// { dg-do assemble  }
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 9 Aug 2000 <nathan@codesourcery.com>

// We failed to spot where a typename T::t didn't actually declare
// anything. [7.1.5.3]/1

template<class LB> struct C
{
  typename LB::DataType;      // { dg-error "" } does not declare anything
  typename LB::DataType m;
};

struct B {};
struct A {
  B;                          // { dg-error "" } does not declare anything
  B m;
};
