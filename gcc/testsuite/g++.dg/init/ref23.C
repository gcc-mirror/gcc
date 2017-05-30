// PR c++/80176
// { dg-do compile }

struct X { static void foo(); static void baz(int); static int baz(double); } x;
struct Y { void o(unsigned char); static void o(int); void o(double); } y;
void X::foo() {}
static void bar() {}
void (&r1)() = x.foo;
void (&r2)() = X::foo;
void (&r3)() = bar;
void (&r4)(int) = x.baz;
int (&r5)(double) = x.baz;
void (&r6)(int) = X::baz;
int (&r7)(double) = X::baz;
void (&r8)(int) = y.o;
