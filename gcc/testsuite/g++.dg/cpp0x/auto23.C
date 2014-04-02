// PR c++/46245
// { dg-do compile { target c++11 } }

template<auto f()->int> struct A { };
