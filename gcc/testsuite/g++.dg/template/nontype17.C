// PR c++/28743

template<int> struct A
{
    template<typename> void foo();
};

template<int> template<typename> void A<0>::foo() {} // { dg-error "match" }
