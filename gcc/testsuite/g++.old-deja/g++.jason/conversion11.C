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
  void DoSomething(Ding A);	// { dg-message "Something::DoSomething|no known conversion" } referred to
};

void DoSomething(Ding A);

void foo(Something* pX)
{
  DoSomething(1);		// { dg-error "could not convert" }
  pX->DoSomething(1);		// { dg-error "no matching" } 
  (*pX).DoSomething(1);		// { dg-error "no matching" } 
}
