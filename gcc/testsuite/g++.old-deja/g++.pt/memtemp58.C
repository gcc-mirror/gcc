// { dg-do assemble  }
// GROUPS passed templates membertemplates
template<int N, class T>
struct B {
};

template<int N1, int N2, int N3>
struct D {
    struct E {
        template<int N4, class T>
        static void f(B<N4,T>)
        { }
    };
};

template<int N>
struct A {
    template<int N2, class T, int N3>
    static void f(B<N2,T>, B<N3,T> b)
    {
        typedef typename D<N2,N3,N>::E E;
	E::f(b);
    }
};

void foo()
{
    A<5>::f(B<5,float>(),B<3,float>());
}

