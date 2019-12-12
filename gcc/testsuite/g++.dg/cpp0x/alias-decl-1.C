// { dg-do compile { target c++11 } }

// These also represent tests for printing alias declarations and
// their instantiations.

template<class T, class U> struct A0 {};
template<class T, class U> using AA0 = A0<T, U>;
template<class T> struct AA0<int, T> {}; // { dg-error "specialization" }

template <class U> using Ptr = U*;
template<class U> struct Ptr<U*> {}; // { dg-error "specialization" }

struct A {
    using A = int;  // { dg-error "11:ISO C\\+\\+ forbids nested type .A." }
// { dg-error "11:.using A = int. has the same name as" "" { target c++11 } .-1 }  
};
