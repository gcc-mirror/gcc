// PR c++/33461
// { dg-options "-std=gnu++0x" }

template<typename> struct A;

template<typename... T> struct A<T*>  // { dg-error "not expanded|T|not used|T" }
{
  struct B;
};

A<void*> a; // { dg-error "incomplete" }
