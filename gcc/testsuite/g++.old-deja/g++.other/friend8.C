// { dg-do assemble  }
// 
// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Aug 2000 <nathan@codesourcery.com>

// We failed to diagnose when a class friend declaration did not use an
// elaborated type specifier.

struct Y;
struct Z;
struct X
{
  friend class Z;
  friend Y;         // { dg-error "" } friend must use aggr tag
};

