// PR c++/27581
// { dg-do compile }

struct A
{
    template<int> static void foo();
    static void bar() { this->A::foo<0>(); } // { dg-error "unavailable" }
};
