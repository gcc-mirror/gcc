// { dg-do compile }

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 21 Jul 2004 <nathan@codesourcery.com>

// { dg-final { scan-assembler "_ZNK2A1IiEcviEv" } }

template <typename T> struct A1 
{
  operator T () const;  // this one
};

struct A2 : A1<int>
{
  template<typename T> operator T () const;
};

int Foo (A2 const &b)
{
  return b;
}

