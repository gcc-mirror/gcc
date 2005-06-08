// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 6 Jun 2005 <nathan@codesourcery.com>

// PR 20613:uninformative diagnostic
// Origin:  Wolfgang Bangerth <bangerth@dealii.org>

struct B { 
  void f();
}; 
 
struct D : B { 
  using B::f; 
  using B::f;  // { dg-error "repeated" }
}; 
