// Origin: PR c++/51032
// { dg-do compile { target { c++11 && stabs } } }
// { dg-options "-gstabs+" }

template <class C>
struct A {
    template<class U> using B = U*;
    int a;
};

A<int> a;

