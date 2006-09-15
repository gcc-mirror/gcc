// PR C++/29002
// We ICE trying to set the "zero" initializer on the incomplete
//  array

struct A {};
int A::* x[]; // { dg-error "size" }
