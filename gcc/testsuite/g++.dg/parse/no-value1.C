// PR c++/5533
// { dg-do compile }

namespace N
{
    template <class T> struct A{};
}

template <class T> void foo(T) {}

void bar()
{
    foo(N::A); // { dg-error "" }
}
