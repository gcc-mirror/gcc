// PR c++/95719
// { dg-do compile { target c++11 } }

struct S1 { virtual ~S1(); };
struct S2 {
    virtual ~S2();
    virtual void f();
};
struct S3 final: S1, S2 { using S2::f; };
void g(S3 & s) { s.f(); }
