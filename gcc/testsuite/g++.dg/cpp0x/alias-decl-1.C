// { dg-options "-std=c++11" }

// These also represent tests for printing alias declarations and
// their instantiations.

template<class T, class U> struct A0 {};
template<class T, class U> using AA0 = A0<T, U>;
template<class T> struct AA0<int, T> {}; // { dg-error "specialization" }

template <class U> using Ptr = U*;
template<class U> struct Ptr<U*> {}; // { dg-error "specialization" }

struct A {
    using A = int;//{ dg-error "nested|has|same name as|class|in which|declared" }
};
