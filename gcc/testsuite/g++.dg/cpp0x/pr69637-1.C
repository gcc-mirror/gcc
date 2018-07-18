// { dg-do compile { target c++11 } }

template <class T>
int foo () { return 1; }

struct B {
    unsigned c: foo;  // { dg-error "non-integral type" }
};
