// Origin PR c++/51641
// { dg-do compile }

struct A {
    struct B { typedef int X; };
};

template<class B> struct C : A {
    B::X q; // Ok: A::B.
    struct U { typedef int X; };
    template<class U>
        struct D;
};

template<class B>
template<class U>
struct C<B>::D {
    typename U::X r; // { dg-error "" }
};

C<int>::D<double> y;

