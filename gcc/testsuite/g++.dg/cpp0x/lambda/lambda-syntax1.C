// PR c++/46124
// { dg-options -std=c++0x }

void foo() { [] () -> void (); } // { dg-error "returning a function" "returning" }
// { dg-error "expected .\{" "expected" { target *-*-* } 4 }
