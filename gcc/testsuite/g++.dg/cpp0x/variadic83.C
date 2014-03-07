// PR c++/31441
// { dg-do compile { target c++11 } }

template<typename> struct A;

template<typename... T> struct A<T...> { }; // { dg-bogus "cannot expand" "" }

A<int> a; // { dg-bogus "incomplete type" "" }
