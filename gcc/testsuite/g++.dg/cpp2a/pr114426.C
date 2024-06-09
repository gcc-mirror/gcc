// PR c++/114426
// { dg-do compile }
// { dg-additional-options "-O2" }

struct A { virtual ~A (); };
struct B : virtual A { virtual void foo () = 0; };
struct C : B { C () {} };
