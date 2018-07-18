// PR c++/80073
// { dg-do compile { target c++11 } }

struct A {};

struct B : virtual A {};

struct C : B {} c {};
