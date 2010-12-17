// { dg-options -std=c++0x }

int x, &&y = static_cast<int &&>(x);
typedef decltype((y)) myInt;  // `y' is a parenthesized id-expression of type int that is an lvalue
typedef int &myInt;
