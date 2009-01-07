// PR c++/33461
// { dg-options "-std=gnu++0x" }

template<typename> struct A;

template<typename... T> struct A<T*...> // { dg-bogus "cannot expand" "" { xfail *-*-* } }
{
  struct B;
};

A<void*> a; // { dg-bogus "incomplete type" "" { xfail *-*-* } }
