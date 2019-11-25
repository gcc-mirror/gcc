// { dg-do assemble  }
// Copyright (C) 1999 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 1 Sep 1999 <nathan@acm.org>

// [expr.unary.op]/1 says you can dereference all pointers except for pointers
// to cv void.

void fn (void *vp, volatile void *vvp)
{
  *vp;      // { dg-error "3:.void\\*. is not a pointer-to-object type" } not a pointer to object
  *vvp;     // { dg-error "3:.volatile void\\*. is not a pointer-to-object type" } not a pointer to object
  &*vp;     // { dg-error "4:.void\\*. is not a pointer-to-object type" } not a pointer to object
  &*vvp;    // { dg-error "4:.volatile void\\*. is not a pointer-to-object type" } not a pointer to object
}
