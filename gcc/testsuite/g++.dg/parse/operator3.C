// PR c++/9454
// Origin: Wolfgang Bangerth <bangerth@ticam.utexas.edu>
// { dg-do compile }

template <int> struct A
{
    struct B     { operator int (); };
    struct C : B { operator int (); };
};

A<0> a;
