// PR c++/16853

struct A {};
struct B {};

int B::* b;
int A::* a = b; // { dg-error "" }
