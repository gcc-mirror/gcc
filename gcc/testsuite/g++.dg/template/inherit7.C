// Origin: PR c++/48656
// { dg-do compile { target c++11 } }

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
