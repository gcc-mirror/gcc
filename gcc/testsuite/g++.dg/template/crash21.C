// { dg-do compile }

// Origin: Debian GCC maintainers <debian-gcc@lists.debian.org>
//	   Wolfgang Bangerth <bangerth@dealii.org>

// PR c++/16706: Dependent type calculation during access checking

template <typename> struct B { 
    B() throw() {} 
    struct S { }; 
    static int i; 
    typedef unsigned short int dummy; 
}; 
 
template <typename _Tp> 
struct allocator: B<_Tp> { 
    template<typename _Tp1> struct rebind 
    { typedef allocator<_Tp1> other; }; 
}; 
 
template<typename T, typename> 
struct X { 
    typename allocator<T>::template rebind<int>::other i; 
    typedef int* dummy; 
}; 
 
template <class T> class A { 
    typedef typename X<T,allocator<T> >::dummy dummy; 
    template <class TP> class XWrapper; 
}; 
 
 
template <class T> 
template <class TP> struct A<T>::XWrapper<TP *> 
{ 
    XWrapper() {} 
    X<int,allocator<int> > x; 
}; 
 
template class A<int>::XWrapper<int *>;
