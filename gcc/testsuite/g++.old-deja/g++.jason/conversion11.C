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
  void DoSomething(Ding A);	// ERROR - referred to
};

void DoSomething(Ding A);

void foo(Something* pX)
{
  DoSomething(1);		// ERROR - 
  pX->DoSomething(1);		// ERROR - 
  (*pX).DoSomething(1);		// ERROR - 
}
