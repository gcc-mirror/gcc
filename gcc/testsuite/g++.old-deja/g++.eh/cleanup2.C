// { dg-do run  }
//  Copyright (C) 1999 Free Software Foundation, Inc.
//  Contributed by Nathan Sidwell 21 Nov 1999 <nathan@acm.org>

// make sure we don't call base dtors, if we failed to call the
// base ctor due to exception throwing

#include <stdio.h>

static bool bad = false;

static int thrower ()
{
  printf ("in %s\n", __PRETTY_FUNCTION__);
  throw 0;
  return 0;
}

struct X
{
  X (int) throw (int);
  ~X () throw ();
};

X::X (int) throw (int)
  {printf ("in ctor X %s\n", __PRETTY_FUNCTION__); bad = true;}
X::~X () throw ()
  {printf ("in dtor X %s\n", __PRETTY_FUNCTION__); bad = true;}

struct X1 {};
struct Y : X
{
  Y() throw (int);
  ~Y() throw ();
};
Y::Y() throw (int)
  : X(thrower ())   // throws, so X::X is never called
  {printf ("in ctor Y%s\n", __PRETTY_FUNCTION__); bad = true;}
Y::~Y() throw ()
  {printf ("in dtor Y%s\n", __PRETTY_FUNCTION__); bad = true;}

int main ()
{
  try
    {
      Y y;
    }
  catch (...)
    {
      printf ("caught\n");
    }
  return bad;
}
