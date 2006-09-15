// PR C++/29002
// We ICE trying to set the "zero" initializer on the incomplete
//  array

struct A {A();int A::* t;};
A x[]; // { dg-error "size" }
