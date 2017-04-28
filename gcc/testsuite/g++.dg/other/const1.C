// { dg-do compile }

// Copyright (C) 2001 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 26 Dec 2001 <nathan@nathan@codesourcery.com>

// PR 335. Missed diagnostic

struct Foo
{
  unsigned i;
  void Modify(unsigned j) const;
};

void Foo::Modify(unsigned j) const
{
  Foo::i = j;  // { dg-error "assignment of member" }
}
