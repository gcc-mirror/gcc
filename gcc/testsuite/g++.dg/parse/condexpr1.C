// PR c++/10247
// Origin: Lars Gullik Bjønes <larsbj@lyx.org>
// { dg-do compile }

struct A {};

A const foo();

void bar()
{
    A a = foo();
    A b = true ? a : foo();
}
