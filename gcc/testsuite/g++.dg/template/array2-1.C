// { dg-do compile }
// { dg-options "-fabi-version=1" }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 29 Dec 2003 <nathan@codesourcery.com>

// PR c++/13494. ICE

template<typename T>
int foo(int d[][4])
{
  return d[0][0];
}

