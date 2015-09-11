// PR c++/65695
// { dg-do compile { target c++11 } }

struct Foo;

struct Bar
{
    using MemberFuncT = int (Foo::*)();

    MemberFuncT h_;
    constexpr Bar(MemberFuncT h) : h_{h}
    {
    }
};

struct Foo
{
    int test()
    {
        return -1;
    }

    static constexpr Bar bar {&Foo::test};
};

constexpr Bar Foo::bar;
