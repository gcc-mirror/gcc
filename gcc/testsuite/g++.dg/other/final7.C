// PR c++/90909
// { dg-do run { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for cassert" { ! hostedlib } }

#include <cassert>

struct S1 { virtual bool f() { return false; } };
struct S2: S1 { virtual bool f() { return true; } };
struct S3: S2 { using S1::f; };
struct S4 final: S3 { void g(); };
void S4::g() { assert (f() == true); }
int main() { S4().g(); }
