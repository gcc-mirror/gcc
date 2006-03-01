// { dg-do compile }
// { dg-options "-O2" }

struct A
{
    A();
    virtual A foo() const;
};

void bar()
{
    const A& a=A();
    a.foo();
}
