// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin PR c++/42225
// { dg-options "-std=c++0x" }
// { dg-do compile }

template<class T>
struct A
{
    typedef T I;
    static const char *i;
};

template<class T, int>
struct B
{
    typedef T TT;
    typedef decltype(TT::i)  TT_I0;
    typedef decltype(&TT::i) TT_I1;
    typedef decltype(*TT::i) TT_I2;
    typedef A<TT_I0> TA0;
    typedef A<TT_I1> TA1;
    typedef A<TT_I2> TA2;
};

template<class T>
void
foo()
{
    typedef T TT;
    typedef decltype(TT::i)  TT_I0;
    typedef decltype(&TT::i) TT_I1;
    typedef decltype(*TT::i) TT_I2;
    typedef A<TT_I0> TA0;
    typedef A<TT_I1> TA1;
    typedef A<TT_I2> TA2;
}

int
main()
{
    foo<A<int> >();
}

