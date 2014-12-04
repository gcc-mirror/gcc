// Origin: PR c++/51633
// { dg-do compile { target c++11 } }

struct A
{
    ~A();
};

struct B
{
    A a;
    constexpr B() {}
};

struct A1
{
    int a;
    ~A1();
};

struct B1
{
    A1 a1;
    constexpr B1() {} // { dg-error "B1::a1" }
};
