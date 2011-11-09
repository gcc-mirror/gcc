// Origin: PR c++/51032
// { dg-options "-std=c++0x -gstabs+" }

template <class C>
struct A {
    template<class U> using B = U*;
    int a;
};

A<int> a;

