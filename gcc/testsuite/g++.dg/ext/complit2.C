// PR c++/12726
// Origin: Vladimir Zidar  <mr_W@mindnever.org>
// Reduced version: Volker Reichelt  <reichelt@igpm.rwth-aachen.de>
// { dg-options "" }

struct A
{
    A();
    A(const A&);
    A(int);
};

struct B
{
    A a;
};

void foo()
{
    B b;
    b = (B){0};
}
