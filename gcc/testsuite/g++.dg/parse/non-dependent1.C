// PR c++/8921
// Origin: Chin-Lung Chang <evirt@iis.sinica.edu.tw>
// { dg-do compile }

struct A
{
    template <typename T> void foo();
};

template <typename T> void bar(A& a)
{
    a.foo<T>();
}

void baz()
{
    A a;
    bar<int>(a);
}
