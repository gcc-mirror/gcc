// { dg-do compile }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 30 Nov 2001 <nathan@nathan@codesourcery.com>

// PR 3381

namespace N {
  template<class T>
  class A { };
}

template class N::A<unsigned>; // this works (by itself)
template class ::N::A<int>; // but this doesn't
