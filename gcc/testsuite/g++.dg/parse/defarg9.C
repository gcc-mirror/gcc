// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 6 Jun 2005 <nathan@codesourcery.com>

// PR 21903:Reject legal with default arg confusion
// Origin:  Wolfgang Bangerth <bangerth@dealii.org>


struct O { 
  template<typename T> struct B { 
    void set (T, bool=true); 
  }; 
  
  struct D : public B<int> {}; 
}; 

void x () 
{ 
  O::D d; 
  d.set(1); 
}
