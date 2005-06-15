// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Jun 2005 <nathan@codesourcery.com>

// PR 20678: ICE on error message
// Origin:  Andrew Pinski pinskia@gcc.gnu.org

struct a
{
  a(const a&);
};
struct b
{ // { dg-error "cannot bind packed field" }
  a aa __attribute__((packed));
};
struct c
{
  b bb;
  c(const b& __a): bb(__a) {} // { dg-error "synthesized" }
};
