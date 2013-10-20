// PR c++/58466
// { dg-do compile { target c++11 } }

template<char, char...> struct A;

template<typename> struct B;

template<char... C> struct B<A<C...>> {};

B<A<'X'>> b;        // { dg-error "incomplete type" }
