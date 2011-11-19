// PR c++/26256
// { dg-do compile }

struct A { int f; };
struct B { int f; };
struct C : A, B { using B::f; };

struct D : C
{
    void g() { f = 1; }
};
