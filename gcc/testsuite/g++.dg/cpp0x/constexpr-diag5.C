// Origin: PR c++/51633
// { dg-do compile { target c++11 } }

struct A
{
    constexpr A() {}
    ~A();
};

struct B
{
    A a;
    A b;
    A c;
    constexpr B() {}
};

struct C
{
    A a;
    constexpr C() {}
};

struct D
{
    constexpr D() { return;} // { dg-error "does not have empty body" }
};

struct D1
{
    A a;
    constexpr D1() { return;} // { dg-error "does not have empty body" }
};

struct D2
{
    A a;
    A b;
    constexpr D2() { return;} // { dg-error "does not have empty body" }
};

struct D3
{
    A a;
    A b;
    A c;
    constexpr D3() { return;} // { dg-error "does not have empty body" }
};
