// { dg-do compile }

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 21 Jul 2004 <nathan@codesourcery.com>

// { dg-final { scan-assembler "_ZNK1AcviEv" } }
// { dg-final { scan-assembler-not "_ZNK1VcviEv" } }

struct V 
{
  operator int () const;
};

struct A : virtual V
{
  operator int () const; // this one
};

struct B1 : A, virtual V
{
};

struct B2 : virtual V, A
{
};


int Foo (B1 const &b)
{
  return b;
}
int Foo (B2 const &b)
{
  return b;
}
