// PR c++/66061
// { dg-do compile { target c++14 } }

template<int...>
int x = 1;

template<int n, int... m>
int x<n, m...> = 1;
