// Build don't link:

// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 20 May 1999 <nathan@acm.org>

// Anon unions cannot have user defined member functions
// [class.union/2].  Make sure we spot that.


struct A
{
  union
  {  // ERROR - anon union cannot have member fns
    void bad();
  };
};
