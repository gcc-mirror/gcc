// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 13 Oct 2005 <nathan@codesourcery.com>

// PR 22551:ICE
// Origin:  Johnny Casey <emailwastefilter-bugzillagccorg@yahoo.com>

const int B = 0x80000000;

#define b(x) (B + x)


int Foo (int error)
{
  switch (error)
  {
  case b (1): return 0; // { dg-error "overflow" "" }
  case b (2): return 0; // { dg-error "overflow" "" }
  case b (3): return 0; // { dg-error "overflow" "" }
  case b (4): return 0; // { dg-error "overflow" "" }
  case b (5): return 0; // { dg-error "overflow" "" }
 }
}
