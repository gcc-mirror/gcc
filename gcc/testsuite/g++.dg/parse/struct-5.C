// PR c++/55368

struct A { struct B *C,; };  // { dg-error "stray" }
