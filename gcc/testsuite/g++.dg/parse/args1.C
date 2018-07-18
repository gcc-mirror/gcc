// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 9 Jul 2003 <nathan@codesourcery.com>

// PR c++ 11050. Accepted ill-formed


void Foo (int)
{
  Foo(2 2); // { dg-error "expected" }
}
