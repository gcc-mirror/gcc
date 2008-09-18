// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 14 Jun 2005 <nathan@codesourcery.com>

// PR 20678: ICE on error message
// Origin:  Andrew Pinski pinskia@gcc.gnu.org

// NOTE: This test assumes packed structure layout differs from unpacked
//       structure layout.  This isn't true, e.g., with the default
//       arm-none-elf options.
// { dg-options "-mstructure-size-boundary=8" { target arm-*-* } }

struct a
{
  int m;
  a(const a&);
};
struct b
{ // { dg-error "cannot bind packed field" "" { target { ! default_packed } } }
  char c;
  a aa __attribute__((packed)); // { dg-warning "attribute ignored" "" { target default_packed } }
};
struct c
{
  b bb;
  c(const b& __a): bb(__a) {} // { dg-message "synthesized" "" { target { ! default_packed } } }
};
