// PR c++/10150
// Origin: Tom Evans <te200@eng.cam.ac.uk>
// { dg-do compile }

template <int I> struct A
{
    template <int> struct B
    {
        enum { e = I * A<I-1>::B }; // { dg-error "" }
    };
};

A<0>::B<0> a; // { dg-error "instantiated" }
