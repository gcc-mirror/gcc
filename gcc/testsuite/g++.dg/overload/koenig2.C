// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 18 Oct 2005 <nathan@codesourcery.com>

// PR 21383
// Origin: Matthew Hall <mahall@ncsa.uiuc.edu>

template <class T>
void dummy(T& t);

void anyfunc(int x);

void Foo ()
{
  anyfunc (&dummy<>); // { dg-error "cannot resolve overload" "" }
}
