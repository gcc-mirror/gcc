// Copyright (C) 2004 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 7 Dec 2004 <nathan@codesourcery.com>

// PR 18803: reject legal
// Origin: Wolfgang Bangerth <bangerth@dealii.org>

struct A { 
  int operator() (); 
}; 
 
template <int> void foo () { 
  A &a = *new A(); 
  const int i = a(); 
} 
