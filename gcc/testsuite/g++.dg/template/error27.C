// PR c++/27211

struct A {};

template<int> void A::foo() {} // { dg-error "member function" }
