// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 27 Dec 2002 <nathan@codesourcery.com>

// We ICE'd

struct c0 {};

struct c1 : virtual c0
{
  virtual c0 &f2();
};

struct c3 : virtual c1
{
  virtual c1 &f2();
};

c1 &c3::f2()
{
  throw 0;
}

struct c4 : virtual c3
{
};
