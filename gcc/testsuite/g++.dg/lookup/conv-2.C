// { dg-do compile }

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 21 Jul 2004 <nathan@codesourcery.com>

// { dg-final { scan-assembler "_ZNK2A1cviEv" } }

struct A1 
{
  operator int () const; // this one
};

struct A2 : A1
{
  template<typename T> operator T () const;
};

int Foo (A2 const &b)
{
  return b;
}

