// DR 1286: An alias template can be equivalent to an underlying template.
// { dg-do compile { target c++11 } }

template <class T, class U> struct same;
template <class T> struct same<T,T> {};

template <class T> struct A {};
template <class T> using B = A<T>;

template <template <class> class T> class C {};

void f(C<B>) { }	    // { dg-final { scan-assembler "_Z1f1CI1AE" } }
same<C<A>, C<B> > s;
