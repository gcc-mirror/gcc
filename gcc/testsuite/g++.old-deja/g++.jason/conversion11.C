// { dg-do assemble  }
// PRMS Id: 8805
// Bug: The two-level conversion is not found when calling a global fn.

class Int {
public:
  Int(int A);
};

class Ding {
public:
  Ding(Int A);
};

class Something {
public:
  void DoSomething(Ding A);	// { dg-error "" } referred to
};

void DoSomething(Ding A);

void foo(Something* pX)
{
  DoSomething(1);		// { dg-error "" } 
  pX->DoSomething(1);		// { dg-error "" } 
  (*pX).DoSomething(1);		// { dg-error "" } 
}
