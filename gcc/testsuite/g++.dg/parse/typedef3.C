// PR c++/3004
// Origin: Travis J.I. Corcoran <tjic@permabit.com>
// { dg-do compile }

struct A { typedef A* Ptr; };

struct A::Ptr; // { dg-error "" }
