// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 21 Mar 2003 <nathan@codesourcery.com>

// PR 9708. We accepted a local class

template <typename T> class X {};

void fn ()
{
  class L {};
  X<L> f; // { dg-error "uses local type|trying to instantiate|no type|invalid type" "" }
}
