// { dg-do compile }

// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 24 Aug 2004 <nathan@codesourcery.com>
// Origin: Wolfgang Bangerth  <bangerth@dealii.org>

// Bug 16889:Undetected ambiguity.

struct B { 
  int i; // { dg-message "int B::i" "" }
}; 
 
struct B1 : virtual B {}; 
struct B2 : B {}; 
struct BB : B1, B2 {}; 
 
int i = BB().i;  // { dg-error "ambiguous" "" }
