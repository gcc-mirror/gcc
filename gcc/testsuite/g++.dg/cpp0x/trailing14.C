// PR c++/65775
// { dg-do compile { target c++11 } }
// { dg-options "-Wignored-qualifiers -Wno-volatile" }

using Qi = int const volatile;
Qi q1();           // { dg-warning "1: type qualifiers ignored" }
auto q2() -> Qi;   // { dg-warning "1: type qualifiers ignored" }

using Fi = int();
Fi f1();           // { dg-error "1: 'f1' declared as function returning a function" }
auto f2() -> Fi;   // { dg-error "1: 'f2' declared as function returning a function" }

using Ai = int[5];
Ai a1();           // { dg-error "1: 'a1' declared as function returning an array" }
auto a2() -> Ai;   // { dg-error "1: 'a2' declared as function returning an array" }
