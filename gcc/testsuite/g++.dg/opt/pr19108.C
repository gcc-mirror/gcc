// PR tree-optimization/19108
// This used to abort due to not handing RANGE_EXPR in SRA.

// { dg-do compile }
// { dg-options "-O" }

struct A
{
    int i[6];
    A () : i() {}
};

struct B
{
    A a;
    B(const A& x) : a(x) {}
};

B b=A();
