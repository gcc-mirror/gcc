// Origin PR c++/51475
// { dg-do compile { target c++11 } }

#include <initializer_list>

struct A
{
    A(int*);
};

struct B
{
    const std::initializer_list<A>& x;
};

B b = {{1}}; // { dg-error "" }
