// PR c++/105425
// { dg-do compile { target c++11 } }

template<bool> struct when;
template<class, class> struct A;
template<class T, bool K> struct A<T, when<K>>;            // #1
template<class T>         struct A<T, when<T(2) == 1>> {}; // #2
A<int, when<true>> a1; // { dg-error "incomplete" }
A<int, when<false>> a2;
A<bool, when<true>> a3;
A<bool, when<false>> a4; // { dg-error "incomplete" }
