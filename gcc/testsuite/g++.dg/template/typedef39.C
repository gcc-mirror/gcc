// PR c++/50852

template<int d> class A;
template<class T> struct B {typedef int K;typedef int L;};
template<class U,class V> struct C
{
    typedef typename U::L X;
    typedef A<X::a-1> W;
};
template<class U,int d> struct D
{
    typedef typename U::L X;
    typedef A<X::a-1> W;	// { dg-error "not a member" }
};
template class D<B<A<1> >,3>;
