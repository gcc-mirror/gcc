// { dg-do assemble  }
// GROUPS niklas uncaught default-construct
struct A { A (int); };
struct B : A {}; // { dg-message "note" "" { target c++17_down } } without ctor // ERROR - candidates
void f () { B (0); }// { dg-error "match" "" { target c++17_down } } .*
