// PR c++/9488
// Origin: Wolfgang Bangerth <bangerth@ticam.utexas.edu>
// { dg-do compile }

struct A
{
    template <typename> void foo() {}
};

template <typename T> struct B
{
    void bar() { A().foo<T>(); }
};

template struct B<int>;
