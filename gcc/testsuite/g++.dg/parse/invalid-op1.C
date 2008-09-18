// PR c++/10150
// Origin: Tom Evans <te200@eng.cam.ac.uk>
// { dg-do compile }

template <int I> struct A
{
    template <int> struct B
    {
        enum { e = I * A<I-1>::B }; // { dg-error "dependent-name" "depname" }
         // { dg-message "note" "note" { target *-*-* } 9 }
    };
};

A<0>::B<0> a; // { dg-message "instantiated" }
