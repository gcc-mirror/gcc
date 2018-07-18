// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 16 Sep 2002 <nathan@codesourcery.com>

// Seg faulted.

struct Base 
{
};

struct Derived : Base
{
  void Foo ()
  {
    Base::Baz ();  // { dg-error "is not a member" }
      
  }
};
