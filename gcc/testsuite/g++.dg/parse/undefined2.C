// PR c++/9173
// Origin: <wwieser@gmx.de>
// { dg-do compile }

class A {};

class B
{
    void foo(int,A::X); // { dg-error "" }
};

void B::foo(int,A::X) {} // { dg-error "" }
