// PR c++/33461
// { dg-do compile { target c++11 } }

template<typename> struct A;

template<typename... T> struct A<T*...> // { dg-bogus "cannot expand" "" }
{
  struct B;
};

A<void*> a; // { dg-bogus "incomplete type" "" }
