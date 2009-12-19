// Contributed by Dodji Seketeli <dodji@redhat.com>
// Origin: PR c++/42225
// { dg-do compile }

struct A
{
    typedef int TI;
};

template<class T0>
struct S0
{
    int i;
};

template<class _T, int>
struct S1
{
    typedef _T T;
    typedef typename T::TI TTI;
    typedef S0<TTI> TT0;
    typedef S0<typename T::TI> TT1;
};

template<class T>
void
foo(const T&)
{
    typedef typename T::TI TTI;
    typedef S0<TTI> TT1;
    typedef S0<typename T::TI> TT2;
}

int
main()
{
    A a;
    foo (a);
}

