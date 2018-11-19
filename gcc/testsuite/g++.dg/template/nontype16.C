//PR c++/27962

template<int> struct A
{
    template<typename> void foo();
};

template<> template<struct T> void A<0>::foo() {} // { dg-error "class type|incomplete" }
 
