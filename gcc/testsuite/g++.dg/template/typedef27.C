// Origin: PR c++/42713
// { dg-do compile }

template<class T>
struct S
{
};

template<class T>
struct S0
{
    typedef T TT;
};

template<class U, class V>
struct super_struct : S0<V>
{
    typedef S0<V> super;
};

template<class U, class V, class W>
struct S1 : super_struct<U, V>
{
    typedef super_struct<U, V> super;
    typedef typename super::super Super2;
    typedef typename Super2::TT Super2TT;
    void
    foo()
    {
        S<Super2TT> s1;
    }
};

template<class U, class V>
struct S2 : super_struct<U, V>
{
    typedef super_struct<U, V> super;
    typedef typename super::super Super2;
    typedef typename Super2::TT Super2TT;
    void
    foo()
    {
        S<Super2TT> s1;
    }
};

int
main()
{
    S1<int, S<int>, int> s1;
    s1.foo();
    S2<int, S<int> > s2;
    s2.foo();
}

