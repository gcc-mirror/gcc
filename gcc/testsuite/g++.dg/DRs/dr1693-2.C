// DR 1693, Superfluous semicolons in class definitions
// PR c++/113760
// { dg-do compile }
// { dg-options "-pedantic-errors" }

struct S {
  int a;
  ;	    // { dg-error "extra" "" { target c++98_only } }
};
