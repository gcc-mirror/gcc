// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 1 Sep 1999 <nathan@acm.org>

// [expr.unary.op]/1 says you can dereference all pointers except for pointers
// to cv void.

void fn (void *vp, volatile void *vvp)
{
  *vp;      // ERROR - not a pointer to object
  *vvp;     // ERROR - not a pointer to object
  &*vp;     // ERROR - not a pointer to object
  &*vvp;    // ERROR - not a pointer to object
}
