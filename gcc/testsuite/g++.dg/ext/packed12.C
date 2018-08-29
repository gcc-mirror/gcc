// PR c++/80972

struct A { int i; };
struct B { A a; } __attribute__((packed));

A a = B().a;
