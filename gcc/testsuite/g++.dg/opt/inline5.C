// PR c++/12519

// { dg-do compile }
// { dg-options "-O" }

struct A
{
    ~A();
};

inline const A foo()
{
    A a;
    return a;
}

A bar()
{
    return foo();
}
