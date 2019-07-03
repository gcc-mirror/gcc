// PR c++/90449
// { dg-options "-Wextra -Wno-inaccessible-base" }

struct A { };

struct B : virtual A { };

struct C : A { };

struct D : B, C { }; // { dg-bogus "virtual base 'A' inaccessible in 'D' due to ambiguity" }
