// PR c++/68586
// { dg-do compile { target c++11 } }

enum E { x , y = 1 + (x << 1) };
template<E> struct A {};
A<x> a;

enum E2 : int { x2 , y2 = x2 << 1 };
template<E2> struct A2 {};
A2<x2> a2;

enum class E3 { x3 , y3 = x3 << 1 };
template<E3> struct A3 {};
A3<E3::x3> a3;
