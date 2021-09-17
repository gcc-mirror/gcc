// PR c++/98486
// { dg-do compile { target c++20 } }

template<class T, class U> concept C = __is_same(T, U);

template<C<int>> int v;

template<> int v<int>;
template<> int v<char>; // { dg-error "match" }
