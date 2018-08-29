struct __attribute ((visibility ("hidden"))) A { int i; };
struct B: A { }; // { dg-warning "base" }
