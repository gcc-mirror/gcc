// prms-id: 6610
// There is a bug in vtable thunks with multiple/virtual inheritance.

int fail = 1;
struct B;
struct A { virtual int f(const B*) = 0; int g(const B*); };
int A::g(const B* t) { return f(t); }
struct B : virtual A { B(); int f(const B*); B* B_this; };
B::B() { if (g(this)) fail = 0; }
int B::f(const B* t) { return t == this; }
struct C : B { int f(const B*); int x; };
int C::f(const B*) { return 0; }

int main() { C c; return fail; }
