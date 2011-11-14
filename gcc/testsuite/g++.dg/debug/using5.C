// PR c++/26256
// { dg-do compile }

struct A
{
    int i;
};

struct B
{
    int i;
};

struct C : A, B
{
    using B::i;
    int f ();
};

int C::f()
{
    return i;
}
