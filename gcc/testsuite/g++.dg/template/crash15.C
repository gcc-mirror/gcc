// PR c++/13310

struct A {};

template <typename> void foo()
{
    A a;
    a.foo<int>(); // { dg-error "" }
}
