// PR c++/28743

template<int I> struct A
{
    template<typename T> void foo();
};

template<int I> template<typename T> void A<0>::foo() {} // { dg-error "" }
