// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 1 Apr 2005 <nathan@codesourcery.com>

// { dg-options "-ggdb2" }
// Origin: ivan <ivanr@syncad.com>
//	   pinskia@gcc.gnu.org
// Bug 20505: ICE with -ggdb2

struct b
{
  static const int d;
  virtual bool IsEmpty() const=0;
  int e,c;
};
const int b::d = ((__SIZE_TYPE__)(&((b*)1)->c) - 1);
