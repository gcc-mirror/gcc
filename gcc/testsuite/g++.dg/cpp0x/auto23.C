// PR c++/46245
// { dg-options -std=c++0x }

template<auto f()->int> struct A { };
