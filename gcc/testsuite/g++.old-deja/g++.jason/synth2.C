// PRMS Id: 4623
// Bug: g++ tries and fails to synthesize a copy constructor for D.
// Build don't link:

class A { };
class B: public virtual A { };
class C: public A { };
class D: public B, public C { }; // gets bogus error - bad synthesis
