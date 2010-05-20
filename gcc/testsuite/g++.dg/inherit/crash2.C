// PR c++/30298

struct A {};

struct B : A, A {}; // { dg-error "duplicate base type" }
struct B : A, A {}; // { dg-error "redefinition" }
