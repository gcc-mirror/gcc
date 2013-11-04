// { dg-options "-std=c++11" }
template<typename...> struct A;

template<typename...T> struct A<T> // { dg-error "not expanded|T|" }
{
 static int i;
};

A<char> a; // { dg-error "incomplete" }
A<int> b; // { dg-error "incomplete" }
