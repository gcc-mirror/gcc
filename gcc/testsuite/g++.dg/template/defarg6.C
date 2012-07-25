// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Oct 2005 <nathan@codesourcery.com>

// PR 21353 missing error.
// Origin:Andrew Pinski <pinskia@gcc.gnu.org>

enum X{ a, b, c };

struct C
{
  static void func (X &ref = a); // { dg-error "" }
};

template <typename T>
struct D
{
  static void func (X &ref = a); // not an error at this point
};

void Foo (X & obj)
{
  D<int>::func (obj);

  D<int>::func (); // { dg-error "" }
}

// { dg-prune-output "passing argument" }
