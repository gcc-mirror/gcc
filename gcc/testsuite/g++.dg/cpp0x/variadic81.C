// PR c++/33461
// { dg-options "-std=gnu++0x" }

template<typename> struct A;

template<typename... T> struct A<T*>  // { dg-error "not expanded|note" }
{                                     // { dg-error "not expanded|note" }
  struct B;
};

A<void*> a;
