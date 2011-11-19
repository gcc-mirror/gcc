// { dg-options "-std=c++98 -Wc++11-compat" }

template<int N> struct X {};

X<1 >> 2> x; // { dg-warning "is treated as|suggest parentheses" }

// From cp/parser.c
typedef int Y;
template <int V> struct Foo {};
Foo<Y () >> 5> r; // { dg-warning "is treated as|suggest parentheses" }
