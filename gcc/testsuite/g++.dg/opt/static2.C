// Origin: reichelt@igpm.rwth-aachen.de
// PR 5571
// { dg-options "-O2" }

template <class T> struct A {};

struct B
{
    static A<int> a;
    void f() { a; }
};

A<int> B::a = A<int>();
