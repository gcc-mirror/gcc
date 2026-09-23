// PR c++/126599
// { dg-do compile }

struct A { int a; };
__attribute__((assume_aligned ((int A::*) 0))) int *foo ();
// { dg-warning "'assume_aligned' attribute argument \\\(int A::\\\*\\\)0 is not positive" "" { target c++98_only } .-1 }
// { dg-warning "'assume_aligned' attribute argument \\\(int A::\\\*\\\)nullptr is not positive" "" { target c++11 } .-2 }
