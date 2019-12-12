// PR c++/90449
// { dg-options -Wno-inaccessible-base }

struct A { int a; };

struct B : A { };

struct C : B, A { }; // { dg-bogus "direct base 'A' inaccessible in 'C' due to ambiguity" }
