// { dg-do compile }

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 23 Sep 2004 <nathan@codesourcery.com>
// Origin: Wolfgang Bangerth  <bangerth@dealii.org>

// Follow on from Bug 16889:Undetected ambiguity.

struct B { 
  int f(); // { dg-message "int B::f" "" }
}; 
 
struct B1 : virtual B {}; 
struct B2 : B {};
struct B2_2 : B2 {};
struct BB : B1, B2_2 {}; 
 
int i = BB().f();  // { dg-error "ambiguous" "" }
