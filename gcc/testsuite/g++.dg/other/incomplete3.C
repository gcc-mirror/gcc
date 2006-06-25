//PR c++/28054

struct A;

struct B
{
    friend A : 2; // { dg-error "incomplete type" }
};

