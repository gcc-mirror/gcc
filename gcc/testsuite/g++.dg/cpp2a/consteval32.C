// { dg-do compile { target c++20 } }

consteval int foo ()  { return 42; }
int bar () { return (*(&foo)) (); } // { dg-error "taking address" }
