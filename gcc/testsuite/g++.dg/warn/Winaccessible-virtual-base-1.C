// PR c++/90449
// { dg-options -Wextra }

struct A { };

struct B : virtual A { };

struct C : A { };

struct D : B, C { }; // { dg-warning "virtual base 'A' inaccessible in 'D' due to ambiguity" }
