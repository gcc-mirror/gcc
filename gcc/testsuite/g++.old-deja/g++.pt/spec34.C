// { dg-do assemble  }

// Copyright (C) 2000 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 17 Jan 2001 <nathan@codesourcery.com>

// Bug 1632. In copying default args from a template to a specialization, we
// lost the object's CV quals, leading to an utterly confusing error message.

struct X;

template <int dim> struct Y
{
  X *f (int i = 0) const;
};

template <> X *Y<2>::f (int i) const
{
  return f (i);
}
