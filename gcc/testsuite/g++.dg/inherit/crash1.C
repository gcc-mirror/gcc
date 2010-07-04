// PR c++/30298

union A {};

struct B : A {}; // { dg-error "fails to be a struct or class type" }
struct B : A {}; // { dg-error "redefinition" }
