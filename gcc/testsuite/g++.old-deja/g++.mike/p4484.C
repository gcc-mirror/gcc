// Build don't link:
// prms-id: 4484

class A {
  char buf[64];
};

typedef void (A::*pmf)();
typedef void (A::*pmfc)() const;

pmfc p = (pmfc)(pmf)0;

class B {
};

class D : public A, public B {
};

typedef int (B::*bmfp)();
typedef int (D::*dmfp)();

bmfp foo;

void bar(dmfp a) {
  bar(foo);
}
