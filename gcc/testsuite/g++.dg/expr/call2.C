// { dg-do compile }

// Copyright (C) 2003 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 11 Sep 2003 <nathan@codesourcery.com>
// Origin: Wolfgang Bangerth <bangerth@dealii.org> 
// PR c++/12184. ICE

class C; 
class D;
bool mm(D); 
 
void g(C& f) { 
  mm(f); // { dg-error "parameter" "" }
}
