// PR c++/15696

struct A {};

typedef void (A::*ftype)();

void foo() { A().*ftype(); } // { dg-error "" }
