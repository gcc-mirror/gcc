/* { dg-do compile } */
/* { dg-options "-O2" } */

struct A {};
struct B { virtual A foo () const; };
struct C { A foo () const; };
struct D : virtual B { A foo () const { return A(); } };
struct F : D { virtual int bar () const; };
int F::bar () const { return 0; }
A C::foo () const { return A (); }
