// PR c++/46124
// { dg-do compile { target c++11 } }

void foo() { [] () -> void (); } // { dg-error "returning a function" "returning" }
// { dg-error "expected .\{" "expected" { target *-*-* } .-1 }
