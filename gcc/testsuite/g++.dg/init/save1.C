// PR c++/8748
// We were wrapping the ARRAY_REF in a SAVE_EXPR, causing us to try to make a bitwise
// copy of b[0].

struct A
{
    int i;
};

struct B : A
{
    virtual ~B();
};

struct C
{
    B b[1];
};

void foo() { C().b[0].i; }
