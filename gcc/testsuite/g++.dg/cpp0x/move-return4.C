// PR c++/87530
// { dg-do compile { target c++11 } }

struct Base { };

template<typename T>
struct A : Base
{
    A();
    A(Base&&);
};

A<int> foo()
{
    A<double> v;
    return v;
}
