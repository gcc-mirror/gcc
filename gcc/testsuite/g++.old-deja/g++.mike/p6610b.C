// { dg-do run  }
// prms-id: 6610

int fail = 1;
struct B;
struct A { virtual int f(const B*) = 0; int g(const B*); };
int A::g(const B* t) { return f(t); }
struct B : virtual A { int f(const B*); B* B_this; };
int B::f(const B* t) { return t == this; }
struct S1 { };
struct C : virtual S1, virtual B, virtual A { C(); };
C::C() { if (g(this)) fail = 0; }
struct D : virtual B, virtual A, C { };

int main() { D d; return fail; }
