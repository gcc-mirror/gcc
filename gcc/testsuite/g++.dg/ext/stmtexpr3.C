// PR c++/16112
// { dg-options "" }

struct A
{
    A();
};

A foo() { return ({ A(); }); }
