// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/42225
// { dg-do compile }

template<class T>
struct A
{
    typedef T I;
};

template<class T, int>
struct B
{
    typedef T TT;
    typedef typename TT::I TT_I;
    typedef A<TT_I> TA;
};

template<class T>
void
foo()
{
    typedef T TT;
    typedef typename TT::I TT_I;
    typedef A<TT_I> TA;
}

int
main()
{
    foo<A<int> >();
}

