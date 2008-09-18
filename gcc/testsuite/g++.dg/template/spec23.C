// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 2 Apr 2005 <nathan@codesourcery.com>

// PR 20723
// Origin: Andrew Pinski <pinskia@gcc.gnu.org>
//         Nathan Sidwell <nathan@gcc.gnu.org>

struct Foo
{
  template <typename T>
  Foo (const T &); // { dg-message "T = Bar" }
};

struct Bar
{
  template <typename T>
  operator T () const;   // { dg-message "T = Foo" }
};

Foo Quux (Bar const &b)
{
  return b; // { dg-error "ambiguous" }
}


