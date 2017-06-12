// { dg-do compile }

// Copyright (C) 2001, 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 29 Dec 2001 <nathan@codesourcery.com>

// PR 4361. Template conversion operators were not overloaded.

struct C
{
  template <typename T2> operator T2 ()
  {
    return 1;
  }
  int Foo ()
  {
    return operator int ();
  }
};

struct D
{
  int Foo ()
  {
    return operator int (); // { dg-error "not defined" }
  }
};

