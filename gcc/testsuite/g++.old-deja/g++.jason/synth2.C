// { dg-do assemble  }
// PRMS Id: 4623
// Bug: g++ tries and fails to synthesize a copy constructor for D.

class A { };
class B: public virtual A { };
class C: public A { };
class D: public B, public C { }; // { dg-bogus "" } bad synthesis
