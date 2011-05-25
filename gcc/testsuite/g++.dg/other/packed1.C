// { dg-do run { xfail sh-*-* lm32-*-* } }

// NMS:2003-04-21 this fails on strict aligned architectures again,
// the patch was reverted because it broke something more important.

// Copyright (C) 2002 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 8 Aug 2002 <nathan@codesourcery.com>

// WRS SPR 63496, lost packed attribute when accessing a packed
// field. This matters on aligned architectures like sh

struct thing { int m; };

struct pod
 {
   char a;
   thing m __attribute__ ((packed)); // { dg-warning "attribute ignored" "" { target default_packed } }
 };

int main ()
{
  thing t;
  pod p;
  
  p.m = t; /* runtime bus error here */

  return 0;
  
}
