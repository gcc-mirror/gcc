// { dg-do assemble  }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 26 Feb 2000 <nathan@codesourcery.com>

// template functions can be distinguished by return type alone. The return
// type may also be a template parameter. 

template <typename C> C foo ();    // { dg-bogus "" } 

void g ()
{
  int (*pfn1) () = &foo;    // { dg-bogus "" } 
  void (*pfn2) () = &foo;   // { dg-bogus "" } 
}
