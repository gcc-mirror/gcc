// PR c++/31441
// { dg-options "-std=gnu++0x" }

template<typename> struct A;

template<typename... T> struct A<T...> { }; // { dg-error "cannot expand" }

A<int> a; // { dg-error "incomplete type" }
