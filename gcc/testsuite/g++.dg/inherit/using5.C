// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 8 Jun 2005 <nathan@codesourcery.com>

// PR 19496: Missing error during parsing.
// Origin:  Volker Reichelt <reichelt@gcc.gnu.org>

template<int> struct A
{
  A::A; // { dg-error "not a base" }
};

struct B
{
  void f ();
  using B::f; // { dg-error "not a base" }
};

