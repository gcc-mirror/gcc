// PR c++/21025

template<int N> struct X { char x[N]; };
template<typename T> X<1 + sizeof(T) - sizeof(T)> F(T const &);
template<int N> struct S { int d() { F(1); } };

