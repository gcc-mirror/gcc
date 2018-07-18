// PR c++/33461
// { dg-do compile { target c++11 } }

template<typename> struct A;

template<typename... T> struct A<T*...> // { dg-error "" }
{
  struct B;
};

A<void*> a;
