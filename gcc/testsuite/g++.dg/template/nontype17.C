// PR c++/35282

template<int> struct A
{
    template<int> void foo();
};

template<> template<int> void A<0>::foo() {}
