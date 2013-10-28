// PR c++/46245
// { dg-options -std=c++11 }

template<auto f()->int> struct A { };
