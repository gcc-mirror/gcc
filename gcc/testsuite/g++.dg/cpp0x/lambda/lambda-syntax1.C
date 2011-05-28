// PR c++/46124
// { dg-options -std=c++0x }

void foo() { [] () -> void (); } // { dg-error "returning a function" }
// { dg-error "expected .\{" "" { target *-*-* } 4 }
