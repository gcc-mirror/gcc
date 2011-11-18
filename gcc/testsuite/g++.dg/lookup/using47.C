// PR c++/51152
// { dg-do compile }

struct A
{
    int a;
};

struct B
{
    int b1;
    int b2;
    A b3;
};

struct C : B
{
    typedef int R;
    typedef int S;
    typedef int T;
    using B::b1;
    using B::b2;
    using B::b3;
    void f()
    {
        b3.a;
	b3.~A();
    }
};
