// PR c++/98832
// { dg-do compile { target c++20 } }

template<class T, class U> struct X { U u; };
template<class T> using Y = X<int, T>;
Y y{0};
