// { dg-do assemble  }
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 7 Nov 2000 <nathan@codesourcery.com>
// Origin: bug 510 wolfgang.bangerth@iwr.uni-heidelberg.de

struct Example {
template <class U> void operator= (U);
};

template <>
void Example::operator=<double> (double);
