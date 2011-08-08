// Copyright (C) 2005 Free Software Foundation, Inc.
// Contributed by Nathan Sidwell 7 Jul 2005 <nathan@codesourcery.com>

// Origin:Wolfgang Bangerth <bangerth@dealii.org>
// PR 21799: deduction of cvqualifiers on member functions was wrong

template <class T> void f (T &,       void (T::*)()      ); // { dg-message "note" }
 
struct X { 
    void g() const {}
}; 
 
const X *x; 
 
int main () { 
  f (*x, &X::g);  // {  dg-error "no matching function" }
  // { dg-message "(candidate|incompatible cv-qualifiers)" "candidate note" { target *-*-* } 16 }
} 
