// PR c++/121524
// { dg-do compile { target c++11 } }

typedef unsigned int T;
struct A { unsigned a[8]; unsigned b; };
struct B { T foo[8] [[gnu::aligned (32)]]; };
struct C { T a[8]; T b; };
static_assert (sizeof (C) == sizeof (A), "");
