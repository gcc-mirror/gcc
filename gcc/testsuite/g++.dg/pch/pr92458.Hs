// PR c++/92458
// { dg-options "-std=c++2a" }

template<typename T> concept C = sizeof(T) > 1;
template<typename T> struct S { };
template<typename T> requires C<T> struct S<T> { };
template<typename T> requires (!C<T>) struct S<T> { };
