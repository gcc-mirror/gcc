// PR c++/78647
// { dg-do compile { target c++11 } }
// { dg-options "-w" }

struct A;
void foo ();
void f() { alignas (foo (A)); } // { dg-error "expected" "" }
