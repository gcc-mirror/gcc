// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 31 Dec 2002 <nathan@codesourcery.com>

// ICE with covariant thunks.

struct c0 {};

struct c1 : virtual c0
{
  virtual c0 &f2 ();
};

struct c2 
{
  int m;
};

struct c3 : virtual c0, virtual c1, c2
{
  virtual c1 &f2 ();
};

c1 &c3::f2 ()
{
  throw 0;
}

struct c4 : virtual c3, virtual c0, virtual c1 {};

struct c8 : virtual c2, virtual c0 {};

struct c12 : virtual c4, virtual c3, virtual c8 {};
