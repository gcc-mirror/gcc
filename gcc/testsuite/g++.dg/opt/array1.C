// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 29 Nov 2004 <nathan@codesourcery.com>

// PR 18672:ICE gimplifying incomplete array type.
// Origin: Magnus Fromreide <gcc@magfr.user.lysator.liu.se>

struct A;

struct D {
  static A ary[];
};
extern A ary[];

void Foo (A const *);

void Bar ()
{
  Foo (D::ary);
  Foo (::ary);
}
