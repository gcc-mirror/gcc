// { dg-do compile }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 26 Dec 2001 <nathan@nathan@codesourcery.com>

// PR 196. Misleading diagnostic

namespace N
{
  class B { friend void operator>>(int, class B); };
  class N { friend void operator>>(int,class N); };
} 
void N::operator>>(int, N::B)  // { dg-error "no type `B' in `N::N'" "" }
{ } // { dg-error "" "" }
