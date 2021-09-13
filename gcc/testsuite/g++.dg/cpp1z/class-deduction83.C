// PR c++/96474
// { dg-do compile { target c++17 } }

template <typename = void>
struct A
{
    template <typename = void>
    struct B
    {
    };
};
    
A<>::B b;
