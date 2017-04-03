// PR c++/79463
// { dg-options "-g" }
// { dg-do compile { target c++14 } }

struct A;
extern A a; // { dg-error "'a' has incomplete type" }
template < int > int f = a.x;
