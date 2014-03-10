// PR c++/37208: SFINAE and deleted functions.

// { dg-do compile { target c++11 } }
template<int> struct A { };

template<typename T>
int& int_if_addable(A<sizeof((*(T*)0) + (*(T*)0))>*);

template<typename T>
float& int_if_addable(...);

struct X { };

struct Y { };
Y operator+(Y, Y);

struct Z { };
Z operator+(Z, Z) = delete;

void f()
{
 float& x = int_if_addable<X>(0);
 int& y = int_if_addable<Y>(0);
 float& z = int_if_addable<Z>(0);
}
