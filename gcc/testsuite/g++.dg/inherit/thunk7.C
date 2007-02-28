// { dg-do run }

#include <stdlib.h>

class A
{
    int a, b;
public:
    virtual void foo (int a, int b, int c, int d);
};

class B
{
    int c, d;
public:
    virtual void bar (int a, int b, int c, int d);
};

class D : public virtual A, public virtual B
{
    int e, f;
};

void A::foo(int a, int b, int c, int d)
{
    if (a != 1 || b != 2 || c != 3 || d != 4)
	abort ();
}

void B::bar (int a, int b, int c, int d)
{
    if (a != 5 || b != 6 || c != 7 || d != 8)
	abort ();
}

class C: private D, public virtual A, public virtual B
{
public:
    virtual void foo (int a, int b, int c, int d) { A::foo (a, b, c, d); D::A::foo (a, b, c, d); }
    virtual void bar (int a, int b, int c, int d) { B::bar (a, b, c, d); D::B::bar (a, b, c, d); }
};

C c1;
C *c2 = &c1;
A *c3 = &c1;
B *c4 = &c1;

int main()
{
    c2->foo (1, 2, 3, 4);
    c2->bar (5, 6, 7, 8);
    c3->foo (1, 2, 3, 4);
    c4->bar (5, 6, 7, 8);
    return 0;
}
