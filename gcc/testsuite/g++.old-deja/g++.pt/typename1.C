// { dg-do assemble  }

template<class T>
struct A {
    typedef T T1;
};

template<class T>
struct B {
    typedef T T2;
};

template<class T>
struct C {
};

template<class E>
C<typename E::T2::T1>
foo (E)
{
    return C<typename E::T2::T1>();
}

void test()
{
    foo(B<A<int> >());
}
