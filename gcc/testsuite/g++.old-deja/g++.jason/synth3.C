// { dg-do assemble  }
// Testcase for wrongful generation of copy constructor.

class A { };
class B: virtual private A { };
class D: public B { };		// { dg-bogus "" } 
