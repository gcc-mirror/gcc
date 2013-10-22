// Origin: PR c++/51032
// { dg-skip-if "No stabs" { aarch64*-*-* mmix-*-* *-*-aix* alpha*-*-* hppa*64*-*-* ia64-*-* *-*-vxworks* } { "*" } { "" } }
// { dg-options "-std=c++11 -gstabs+" }

template <class C>
struct A {
    template<class U> using B = U*;
    int a;
};

A<int> a;

