// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 27 Mar 2003 <nathan@codesourcery.com>

// PR 11617: Failed to diagnose missing function.

struct B {};

template <typename T> void Bar ()
{
  T::foo (); // { dg-error "is not a member of" "" }
}

void Foo ()
{
  Bar<B> (); // { dg-message "instantiated" "" }
}
