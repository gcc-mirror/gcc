// { dg-do assemble  }
// PRMS Id: 4484 (bug 5)
// Bug: g++ can't convert between pmf types.

class A;
typedef void (A::*pmf)();
typedef void (A::*pmfc)() const;

pmfc p = (pmfc)(pmf)0;		// { dg-bogus "" } pmf conversion
