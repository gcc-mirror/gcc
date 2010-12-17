// PR c++/28261

struct A {}; // { dg-error "A::A" }

A::A (enum { e }) {} // { dg-error "defined|prototype" }
