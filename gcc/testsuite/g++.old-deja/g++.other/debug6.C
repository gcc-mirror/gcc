// { dg-do assemble  }
// { dg-options "-g -O2" }

//  Copyright (C) 1999 Free Software Foundation, Inc.
//  Contributed by Nathan Sidwell 21 Nov 1999 <nathan@acm.org>

// This causes assember relocation errors

struct X
{
  virtual ~X () {}
};

struct Y
{
  Y (){};
};

void foo ()
{
  X *x = new X;
  x->~X ();
  Y ys[2];
}
