// { dg-do compile }

// Copyright (C) 2001, 2002, 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 29 Dec 2001 <nathan@codesourcery.com>

// PR 5125. ICE

class S
{
  public:
  template <class I> void Foo(int (*f)(S& o) ); 
};

template <class I>
void S::Foo(int (*f)(TYPO&o) ) // { dg-error "Foo|f|TYPO|o" }
{ 
}
