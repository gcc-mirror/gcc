// { dg-do assemble  }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 10 Jan 2001 <nathan@codesourcery.com>

// Bug 595. We failed to clear out some things after seeing a duplicate
// struct definition. That caused us to become inconsistent.

struct X
{
  ~X ();
};
struct S { X a; };  // { dg-message "" } previous defn
struct S { X a; };  // { dg-error "" } redefinition

void c1(S s)
{
}
