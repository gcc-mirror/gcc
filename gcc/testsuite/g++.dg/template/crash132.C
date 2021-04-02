// PR c++/97993
// { dg-do compile { target c++14 } }

template <class T> T a;
template <class T, class A, class=decltype(::new T (A()))> auto foo ();
struct S decltype (foo <int, S>); // { dg-error "" }
