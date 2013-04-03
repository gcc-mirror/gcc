// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 21 Mar 2003 <nathan@codesourcery.com>

// PR 9898. Confusing error message

struct Wrapper {};

void Foo(int const &); // { dg-message "in passing" "" }

void Baz ()
{
  Foo (Wrapper ()); // { dg-error "Wrapper" "" }
}
