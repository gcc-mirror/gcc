// PR c++/66289
// { dg-do compile { target c++11 } }

template<typename T> struct A {};

template<typename T> struct shared_ptr { };
template<typename T> using APtr = shared_ptr<A<T>>;

template<typename T> struct foo;
template<typename T> struct foo<shared_ptr<T>> { };
template<typename T> struct foo<APtr<T>> { };

foo<shared_ptr<A<int>>> aa;
