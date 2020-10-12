// PR c++/94024
// { dg-do compile }

struct A {
    A()
    : a()       // { dg-error "reference type" }
    , b(1)      // { dg-error "incompatible" "" { target { ! c++20 } } }
    , c(0)      // { dg-bogus "" }
    {}

    int &a;
    int b[1];
    char c;
};

template<typename T, typename U>
struct B {
    B()
    : a()       // { dg-error "reference type" }
    , b(1)      // { dg-error "incompatible" "" { target { ! c++20 } } }
    , c(0)      // { dg-bogus "" }
    {}

    T a;
    U b;
    char c;
};

B<int&, int[1]> b;
