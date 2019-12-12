// PR c++/90449

struct A { int a; };

struct B : A { };

struct C : B, A { }; // { dg-warning "direct base 'A' inaccessible in 'C' due to ambiguity" }
