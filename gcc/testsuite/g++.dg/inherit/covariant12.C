// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 27 Feb 2005<nathan@codesourcery.com>

// PR 20232: ICE on invalid

struct T { };

struct S;

struct B
{
  virtual T *Foo (); // { dg-error "overriding" "" }
};

struct D : B
{
  virtual S *Foo (); // { dg-error "invalid covariant" "" }
};
