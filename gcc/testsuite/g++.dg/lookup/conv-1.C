// { dg-do compile }

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 21 Jul 2004 <nathan@codesourcery.com>

// Failed to spot ambiguous conversion

struct A1 
{
  operator int () const; // { dg-error "A1::operator" "" }
};

struct A2
{
  operator int () const; // { dg-error "A2::operator" "" }
};

struct B : A1, A2 
{
};

int Foo (B const &b)
{
  return b; // { dg-error "ambiguous" "" }
}

