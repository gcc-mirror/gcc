// Build don't link:
// Origin: Frank Pilhofer <fp@fpx.de>

struct A {
virtual void f ();
};

struct B : virtual public A {
void f ();
};

struct C : virtual public A {
void f ();
};

struct D : virtual public B, virtual public C {
void f ();
};

struct Di : virtual public B, virtual public C, virtual public D {};
