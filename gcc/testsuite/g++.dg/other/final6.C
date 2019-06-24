// PR c++/90909
// { dg-do link { target c++11 } }

struct S1 { virtual void f() = 0; };
struct S2: S1 { virtual void f() {} };
struct S3: S2 { using S1::f; };
struct S4 final: S3 { void g(); };
void S4::g() { f(); }
int main() { S4().g(); }
