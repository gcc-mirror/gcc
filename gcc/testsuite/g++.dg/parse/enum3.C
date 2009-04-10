// PR c++/28261

struct A {};

A::A (enum { e }) {} // { dg-error "defined|token" }
