// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 22 Jul 2003 <nathan@codesourcery.com>

// PR c++ 10953. ICE

class 
{
  typename::
; // { dg-error "expected" }
 // { dg-error "-:expected" "" { target *-*-* } .+1 }
