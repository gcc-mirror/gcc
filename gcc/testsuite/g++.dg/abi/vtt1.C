// { dg-do compile }

struct A {
};

struct B : virtual public A {
};

B b;

// { dg-final { scan-assembler _ZTT1B } }
