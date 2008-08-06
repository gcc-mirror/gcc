// { dg-options "-std=c++98 -Wc++0x-compat" }

template<int N> struct X {};

X<1 >> 2> x; // { dg-warning "will be treated as|suggest parentheses" }

// From cp/parser.c
typedef int Y;
template <int V> struct Foo {};
Foo<Y () >> 5> r; // { dg-warning "will be treated as|suggest parentheses" }
