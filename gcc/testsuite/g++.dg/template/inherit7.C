// Origin: PR c++/48656
// { dg-options "-std=c++0x" }
// { dg-do compile }

struct A {
 int f();
 int f(int);
};

template <typename> struct B : A
{
};

template <typename T> struct C : B<T>
{
    void
    g()
    {
        A::f();
    }
};
