// { dg-do assemble  }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 23 June 2000 <nathan@codesourcery.com>

// Origin GNATS bug report 10 from Ewgenij Gawrilow <gawrilow@math.TU-Berlin.DE> 
// There is a grammar ambiguity with greater-than compare as a default
// template parameter value or template arg list. 14.2/2 and 14.1/15
// say how to resolve it, but we'd sometimes get it wrong.

template <int> class C { };

void f()
{
C<1> c1;
C<1 & 2> c2;
C<1>2> c3; // { dg-error "" } parse error
C<(1>2)> c4;
C<1 & 2>2> c5; // { dg-error "" } parse error
C<1 & (2>2)> c6;
}

template <int i = 3>4 > class X1 {}; // { dg-error "" } parse error
template <int i = 3&&4>0 > class X2 {}; // { dg-error "" } parse error
template <int i = 3&&4 > class X3 {};
template <int i = (3>4) > class X4 {};
