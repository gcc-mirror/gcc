// { dg-do compile }

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 27 Dec 2002 <nathan@codesourcery.com>

// We ICE'd

struct c0 {};

struct c1 : virtual c0
{
  virtual c0 &f2() volatile;
};

struct c2 
{
  int m;
};

struct c3 : virtual c0, virtual c1, c2
{
  virtual c1 &f2() volatile;
};

struct c4 : virtual c3, virtual c0, virtual c1
{
  int m;
};

struct c6 : c0, c3, c4
{ // { dg-warning "direct base" "" }
  virtual c1 &f2() volatile;
};
